box::use(
  dplyr,
  glue
)

#' Add campaign info to SEA5 anomaly alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df, df_wrangled, df_raw) {
  source_url <- "https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5"

  dplyr$tibble(
    hdx_url = NA_character_,
    source_url = source_url,
    other_urls = NA_character_,
    further_information = as.character(
      glue$glue(
        'Refer to the <a href="{source_url}">ECMWF SEA5 seasonal forecasts for more detailed information.</a>'
      )
    ),
    .rows = nrow(df)
  )
}
