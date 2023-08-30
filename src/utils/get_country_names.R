box::use(dplyr)
box::use(tidyr)
box::use(countrycode)

box::use(gs = ../utils/google_sheets)

#' Add country names to data frame
#'
#' This simple function adds country names to a data frame based on ISO3 code.
#' CERF wanted specific country names on the dashboard, so for some country we
#' did not want to use the inbuilt solutions such as {countrycode}. The function
#' requires the data frame to have an `iso3` column. Any `country` column that
#' already exists will be replaced.
#'
#' @param df Data frame.
#' @returns Data frame with additional `country` name column after `iso3`
#'
#' @export
get_country_names <- function(df) {
  # read the CERF names list
  df_names <- gs$read_gs_file("cerf_dashboard_names")

  # replace existing column name
  if ("country" %in% names(df)) {
    df <- dplyr$select(df, -country)
  }

  dplyr$left_join(df, df_names, by = "iso3") |>
    dplyr$relocate(country, .after = iso3) |>
    dplyr$mutate(
      country = ifelse(
        is.na(country),
        countrycode$countrycode(iso3, origin = "iso3c", destination = "country.name.en"),
        country
      )
    )
}
