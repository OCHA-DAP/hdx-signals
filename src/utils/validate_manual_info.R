box::use(
  src/utils/all_iso3_codes
)


# Function to validate and filter manual dataset
validate_manual_info <- function(df) {

  # Check required columns exist
  required_cols <- c("iso3", "indicator_id", "date")
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

  # Initialize logical vector for valid rows
  valid_rows <- rep(TRUE, nrow(df))

  # Validate iso3 codes
  iso3_valid <- !is.na(df$iso3) &
    df$iso3 != "" &
    !is.null(df$iso3) &
    df$iso3 %in% valid_iso3_codes
  valid_rows <- valid_rows & iso3_valid

  # Validate indicator_id
  indicator_valid <- !is.na(df$indicator_id) &
    df$indicator_id != "" &
    !is.null(df$indicator_id) &
    df$indicator_id %in% valid_indicator_ids
  valid_rows <- valid_rows & indicator_valid

  # Validate date format and parsability
  date_valid <- rep(FALSE, nrow(df))
  for (i in seq_len(nrow(df))) {
    date_val <- df$date[i]

    # Check if date is not NA/NULL/empty
    if (is.na(date_val) || is.null(date_val) || date_val == "") {
      date_valid[i] <- FALSE
      next
    }

    # Check YYYY-MM-DD pattern
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(date_val))) {
      date_valid[i] <- FALSE
      next
    }

    # Try to parse date
    tryCatch({
      date_parsed <- as.Date(date_val, format = "%Y-%m-%d")
      date_valid[i] <- !is.na(date_parsed)
    }, error = function(e) {
      date_valid[i] <- FALSE
    })
  }
  valid_rows <- valid_rows & date_valid

  # Filter and return only valid rows
  df[valid_rows, ]
}
