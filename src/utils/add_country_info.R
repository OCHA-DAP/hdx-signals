box::use(dplyr)
box::use(stringr)

box::use(cs = ./cloud_storage)

#' Add country and region columns
#'
#' Generates columns of country information from a static file stored on Azure
#' by simply performing a left join by iso3 code. Adds country name, region,
#' whether or not it's an HRP country, and lat/lon coordinates.
#'
#' @param df Data frame with an iso3 column
#'
#' @return The input data frame with additional columns added
#'
#' @export
add_country_info <- function(df) {
  df_info <- cs$read_az_file("input/country_info.parquet")
  df_new <- df_info |>
    dplyr$right_join(
      df,
      by = "iso3"
    )

  if (any(is.na(dplyr$select(df_new, country:lat)))) {
    stop(
      stringr$str_wrap(
        paste0(
          "Missing country information, check that the ISO3 code is properly stored ",
          "and handled in `input/country_info.parquet`. If not, fix the data ",
          "by updating `src-raw/update_country_info.R`, then regenerate the alerts."
        )
      ),
      call. = FALSE
    )
  }

  df_new
}
