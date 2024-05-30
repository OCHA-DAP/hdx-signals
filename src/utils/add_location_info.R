box::use(dplyr)
box::use(stringr)

box::use(cs = ./cloud_storage)

#' Add location and region columns
#'
#' Generates columns of location information from a static file stored on Azure
#' by simply performing a left join by iso3 code. Adds location name, region,
#' whether or not it's an HRP location, and lat/lon coordinates.
#'
#' @param df Data frame with an iso3 column
#'
#' @return The input data frame with additional columns added
#'
#' @export
add_location_info <- function(df) {
  df_new <- df_info |>
    dplyr$right_join(
      df,
      by = "iso3"
    ) |>
    dplyr$select(
      iso3, location, region:lon
    )

  if (any(is.na(df_new))) {
    stop(
      stringr$str_wrap(
        paste0(
          "Missing location information, check that the ISO3 code is properly stored ",
          "and handled in `input/location_info.parquet`. If not, fix the data ",
          "by updating `src-raw/update_location_info.R`, then regenerate the alerts."
        )
      ),
      call. = FALSE
    )
  }

  df_new
}

df_info <- cs$read_az_file("input/location_info.parquet")
