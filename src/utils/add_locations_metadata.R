box::use(
  dplyr,
  stringr
)

box::use(cs = src/utils/cloud_storage)

#' Add location metadata columns
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
add_locations_metadata <- function(df) {
  df_new <- df_metadata |>
    dplyr$right_join(
      df,
      by = "iso3"
    )

  if (any(is.na(df_new[, names(df_metadata)]))) {
    stop(
      stringr$str_wrap(
        paste0(
          "Missing location information, check that the ISO3 code is properly stored ",
          "and handled in `input/locations_metadata.parquet`. If not, fix the data ",
          "by updating `src-raw/update_locations_metadata.R`, then regenerate the alerts."
        )
      ),
      call. = FALSE
    )
  }

  df_new
}

df_metadata <- cs$read_az_file("input/locations_metadata.parquet") |>
  dplyr$select(
    iso3, location, region, hrp_location, boundary_source, lat, lon
  )
