box::use(countrycode)
box::use(dplyr)

#' Simple function that returns all ISO3 codes we want to use
#'
#' For help in building out the static datasets for the system, this returns
#' all ISO3 codes we want to handle, including some custom codes not standardized
#' in the `{countrycode}` package.
#'
#' @export
all_iso3_codes <- function() {
  iso3_codes <- countrycode$codelist |>
    dplyr$filter(
      !is.na(iso3c),
      iso3c != "ATA" # drop Antarctica
    ) |>
    dplyr$pull(
      iso3c
    )

  # add in some additional codes not standard
  iso3_codes <- c(iso3_codes, "XKX", "AB9", "LAC")

  iso3_codes
}
