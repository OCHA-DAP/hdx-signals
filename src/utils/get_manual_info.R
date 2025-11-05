box::use(
  src/utils/validate_manual_info,
  src/utils/read_manual_info,
)

box::use(
  logger,
  dplyr
)

#' Read dataset with manually scraped situation and recommendations
#'
#' This function retrieves manually curated information for a specific country,
#' indicator, and date from the manual context information dataset.
#'
#' @param iso3 ISO3 country code (character string)
#' @param indicator_id The Signals indicator identifier (character string)
#' @param date A character string or Date object. The expected date format is YYYY-MM-DD.
#'
#' @return A character vector of length 2 with situation and recommendations, or NULL if no matching data is found.
#' @export
get_manual_info <- function(iso3, indicator_id, date = NULL) {
  # Read data
  data <- read_manual_info$read_manual_info()
  if (is.null(data)) {
    # Return based on indicator type
    if (indicator_id == "acaps_inform_severity") {
      return(dplyr$tibble(
        iso3 = iso3,
        date = as.Date(date),
        manual_info = NA_character_
      ))
    }
    return(NA_character_)
  }

  # Validate data
  data <- validate_manual_info$validate_manual_info(data)
  if (is.null(data) || nrow(data) == 0) {
    logger$log_info("No valid data after validation\n")
    if (indicator_id == "acaps_inform_severity") {
      return(dplyr$tibble(
        iso3 = iso3,
        date = as.Date(date),
        manual_info = NA_character_
      ))
    }
    return(NA_character_)
  }

  # Filter by iso3 and indicator_id
  filtered_data <- data[which(data$iso3 == iso3 & data$indicator_id == indicator_id), ]
  if (nrow(filtered_data) == 0) {
    logger$log_info("No updated or related information found for iso3: ", iso3,
                    " and indicator_id: ", indicator_id, "\n")
    if (indicator_id == "acaps_inform_severity") {
      return(dplyr$tibble(
        iso3 = iso3,
        date = as.Date(date),
        manual_info = NA_character_
      ))
    }
    return(NA_character_)
  }

  # Sort by date (most recent first)
  filtered_data <- filtered_data[order(filtered_data$date, decreasing = TRUE), ]

  # Select row: specific date or most recent
  if (!is.null(date)) {
    exact_date_data <- filtered_data[filtered_data$date == as.Date(date), ]
    if (nrow(exact_date_data) == 0) {
      logger$log_info("No alert day updated info was found for iso3: ", iso3,
                      ", indicator_id: ", indicator_id, " on date: ", date, "\n")
      if (indicator_id == "acaps_inform_severity") {
        return(dplyr$tibble(
          iso3 = iso3,
          date = as.Date(date),
          manual_info = NA_character_
        ))
      }
      return(NA_character_)
    }
    selected_row <- exact_date_data[1, ]
  } else {
    selected_row <- filtered_data[1, ]
  }

  # Extract and return fields based on indicator type
  if (indicator_id == "ipc_food_insecurity") {
    c(as.character(selected_row$text1), as.character(selected_row$text2))
  } else if (indicator_id == "acaps_inform_severity") {
    dplyr$tibble(
      iso3 = iso3,
      date = as.Date(date),
      manual_info = as.character(selected_row$text1)
    )
  } else if (indicator_id %in% c(
    "acled_conflict",
    "jrc_agricultural_hotspots",
    "idmc_displacement_disaster",
    "idmc_displacement_conflict"
  )) {
    selected_row$text1
  }
}
