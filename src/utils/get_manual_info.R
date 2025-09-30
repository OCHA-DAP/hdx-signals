box::use(
  src/utils/validate_manual_info
)


#' Read dataset with manually scraped situation and recommendations
#'
#' This function retrieves manually curated information for a specific country,
#' indicator, and date from the manual context information dataset.
#'
#' @param iso3 ISO3 country code (character string)
#' @param indicator_id The Signals indicator identifier (character string)
#' @param date A character string or Date object. The date in YYYY-MM-DD format to refer to specific alert-related information.
#'
#' @return A character vector of length 2 containing situation and recommendations, or NULL if no matching data is found.
#' @export
get_manual_info <- function(iso3, indicator_id, date) {

  data <- read_manual_info()
  if (is.null(data)) {
    return(NULL)
  }

  data <- validate_manual_info$validate_manual_info(data)

  # Filter data by iso3 and indicator_id, then look for specific date or most recent
  filtered_data <- data[data$iso3 == iso3 & data$indicator_id == indicator_id, ]

  if (nrow(filtered_data) == 0) {
    logger$info("No updated or related information found for iso3: ", iso3, " and indicator_id: ", indicator_id, "\n")
    return(NULL)
  }

  # Determine which row to use
  selected_row <- NULL

  # Check if specific date was provided
  if (!is.null(date)) {
    # Look for exact date match
    exact_date_data <- filtered_data[filtered_data$date == as.Date(date), ]
    if (nrow(exact_date_data) == 0) {
      logger$info("No alert day updated info was found for iso3: ", iso3, ", indicator_id: ", indicator_id, " on date: ", date, "\n")
      return(NULL)
    }
    # logger$info("Found exact date data for ", iso3, " and ", indicator_id, " on date: ", date, "\n")
    selected_row <- exact_date_data[1, ]  # Take first match if multiple
  }

  # Extract fields based on indicator type
  if (indicator_id == "ipc_food_insecurity") {
    # Extract fields
    situation <- selected_row$text1
    recommendations <- selected_row$text2

    # Check if both fields exist and are not empty/NA
    if (is.na(situation) || is.na(recommendations) ||
        situation == "" || recommendations == "") {

      # If both are missing, return NULL
      if ((is.na(situation) || situation == "") &&
          (is.na(recommendations) || recommendations == "")) {
        return(NULL)
      }

      # If only one is available, return it twice
      if (!is.na(situation) && situation != "") {
        return(c(as.character(situation), as.character(situation)))
      } else {
        return(c(as.character(recommendations), as.character(recommendations)))
      }
    }

    return(c(as.character(situation), as.character(recommendations)))

  } else if (indicator_id == "acaps_inform_severity") {
    # Extract field
    if ("text1" %in% names(selected_row) &&
        !is.na(selected_row$text1) &&
        selected_row$text1 != "") {
      info <- as.character(selected_row$text1)
      return(dplyr$tibble(
        iso3 = iso3,
        date = as.Date(date),
        manual_info = info
      ))
    }
    return(NULL)

  } else if (indicator_id %in% c("acled_conflict", "jrc_agricultural_hotspots", "idmc_displacement")) {
    if ("text1" %in% names(selected_row) &&
        !is.na(selected_row$text1) &&
        selected_row$text1 != "") {
      return(selected_row$text1)
    }
    return(NULL)
  }

}
