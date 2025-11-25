box::use(
  src/utils/hs_local,
  src/utils/hs_dry_run,
  cs = src/utils/cloud_storage
)

box::use(
  arrow[read_parquet, write_parquet],
  dplyr,
  logger[log_info],
  purrr
)

#' Get tracking file name
#'
get_tracking_path <- function() {
  filename <- if (hs_dry_run$hs_dry_run()) {
    "summary_input_dry_run.parquet"
  } else {
    "summary_input.parquet"
  }

  path <- file.path("output", filename)

  if (hs_local$hs_local()) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  path
}


#' Read tracking file
#'
#' @export
read_tracking_file <- function() {
  path <- get_tracking_path()

  if (hs_local$hs_local()) {
    if (!file.exists(path)) return(NULL)
    read_parquet(path)
  } else {
    tryCatch(
      cs$read_az_file(path),
      error = function(e) NULL
    )
  }
}


#' Append new rows to tracking file
#'
#' @param new_data Data frame with new tracking data
#' @export
append_tracking_data <- function(new_data) {
  # Validate columns
  required_cols <- c(
    "location_iso3",
    "date_generated",
    "indicator_id",
    "info",
    "manual_info",
    "use_manual_info",
    "summary_long",
    "summary_short",
    "summary_source"
  )

  missing_cols <- setdiff(required_cols, names(new_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!"triage_status" %in% names(new_data)) {
    new_data$campaign_status <- "pending"
  }
  if (!"triaged_at" %in% names(new_data)) {
    new_data$triaged_at <- as.POSIXct(NA)
  }

  path <- get_tracking_path()
  existing_data <- read_tracking_file()

  if (is.null(existing_data)) {
    combined_data <- new_data
    log_info("Creating new tracking file")
  } else {
    combined_data <- dplyr$bind_rows(existing_data, new_data)
    log_info("Appending {nrow(new_data)} rows to tracking file")
  }

  if (hs_local$hs_local()) {
    write_parquet(combined_data, path)
  } else {
    cs$update_az_file(combined_data, path, container = "dev")
  }

  log_info("Tracking file updated: {path}")
}

#' Update tracking file status for triaged signals
#'
#' Updates the summary_input tracking file to mark signals as sent, deleted, or archived
#'
#' @param df Signals dataframe with iso3, date, indicator_id columns
#' @param status New status ("sent", "deleted", or "archived")
#'
#' @export
update_tracking_status <- function(df, status) {
  df |>
    dplyr$group_by(indicator_id) |>
    dplyr$group_split() |>
    purrr$walk(function(df_ind) {
      indicator_id <- unique(df_ind$indicator_id)

      # Read tracking file
      tracking <- read_tracking_file()

      if (is.null(tracking)) {
        return(invisible(NULL))
      }

      # Create keys to match signals
      signal_keys <- paste(df_ind$iso3, df_ind$date, df_ind$indicator_id)
      tracking_keys <- paste(tracking$location_iso3, tracking$date_generated, tracking$indicator_id)

      # Update status for matching signals
      tracking_updated <- tracking |>
        dplyr$mutate(
          campaign_status = dplyr$if_else(
            tracking_keys %in% signal_keys,
            status,
            dplyr$coalesce(campaign_status, "active")
          ),
          triaged_at = dplyr$if_else(
            tracking_keys %in% signal_keys & is.na(triaged_at),
            Sys.time(),
            triaged_at
          )
        )

      # Write back
      path <- get_tracking_path()

      if (hs_local$hs_local()) {
        write_parquet(tracking_updated, path)
      } else {
        cs$update_az_file(tracking_updated, path)
      }
    })
}
