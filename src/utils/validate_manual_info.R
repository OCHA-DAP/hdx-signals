box::use(
  src/utils/all_iso3_codes
)


# Function to validate and filter manual dataset
validate_manual_info <- function(df) {
  # Check required columns exist
  required_cols <- c("iso3", "indicator_id", "date", "text1")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Get validation references
  valid_iso3_codes <- all_iso3_codes$all_iso3_codes()
  valid_indicator_ids <- c(
    "wfp_market_monitor",
    "jrc_agricultural_hotspots",
    "acled_conflict",
    "ipc_food_insecurity",
    "who_cholera",
    "idmc_displacement_conflict",
    "idmc_displacement_disaster",
    "acaps_inform_severity"
  )

  # Combine all validations
  valid_rows <- (df$iso3 %in% valid_iso3_codes) &
    (df$indicator_id %in% valid_indicator_ids) &
    validate_dates(df$date) &
    (!is.na(df$text1) & df$text1 != "")

  # Filter and return only valid rows
  df[valid_rows, ]
}


validate_dates <- function(dates,
                           format = "%Y-%m-%d",
                           pattern = "^\\d{4}-\\d{2}-\\d{2}$") {

  date_valid <- rep(FALSE, length(dates))

  for (i in seq_along(dates)) {
    date_val <- dates[i]

    # Check if date is not NA/NULL/empty
    if (is.na(date_val) || identical(date_val, "")) {
      next
    }

    if (is.null(date_val)) {
      next
    }

    # Check date pattern
    if (!grepl(pattern, as.character(date_val))) {
      next
    }

    # Try to parse date
    tryCatch({
      date_parsed <- as.Date(date_val, format = format)
      date_valid[i] <- !is.na(date_parsed)
    }, error = function(e) {
      date_valid[i] <- FALSE
    })
  }

  return(date_valid)
}


validate_ipc_food_insecurity <- function(df) {

  # Filter for ipc_food_insecurity rows
  ipc_rows <- df$indicator_id == "ipc_food_insecurity"

  # If no IPC rows, return as is
  if (!any(ipc_rows)) {
    return(df)
  }

  # we have already checked text1, so we look at text2
  # if text2 is NULL or empty, copy text1 (situation) to text2 (recommendations)
  text2_empty <- ipc_rows & (is.na(df$text2) | df$text2 == "")
  df$text2[text2_empty] <- df$text1[text2_empty]

  df
}
