box::use(ripc)
box::use(dplyr)

box::use(../../../../src/utils/location_codes)

#' Download raw IPC food insecurity data
#'
#' Downloads raw conflict data from the IPC API. Uses the `{ripc}` package to
#' download the data, which requires an API key. Then just adds an `iso3` column
#' to it.
#'
#' @export
raw <- function() {
  ripc$ipc_get_population()$country |>
    dplyr$filter(
      condition == "A" # only use acute for now, chronic doesn't have date info
    ) |>
    dplyr$mutate(
      iso3 = location_codes$iso2_to_iso3(country),
      p4plus_percentage = phase4_percentage + phase5_percentage,
      date = as.Date(paste(1, analysis_date), format = "%d %b %Y")
    )
}
