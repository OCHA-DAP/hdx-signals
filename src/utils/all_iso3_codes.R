box::use(cs = src/utils/cloud_storage)

#' Simple function that returns all ISO3 codes we want to use
#'
#' For help in building out the static datasets for the system, this returns
#' all ISO3 codes we want to handle, including some custom codes not standardized
#' in the `{countrycode}` package. These are all updated directly in
#' `src-static/update_locations.R`.
#'
#' @export
all_iso3_codes <- function() {
  cs$read_az_file_cached("input/locations.parquet")$iso3
}
