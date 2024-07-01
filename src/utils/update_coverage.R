box::use(cs = src/utils/cloud_storage)

#' Update vector of ISO3 codes covered by dataset
#'
#' Updates coverage data for `indicator_id` based on the vector of ISO3 codes
#' currently returned in the data processing. Stores the data in
#' `output/{indicator_id}/coverage.parquet` as single column file.
#'
#' @param indicator_id Indicator ID
#' @param iso3 Vector of all ISO3 codes from the data. Uses `sort()` and `unique()`
#'     to only store the unique ISO3 codes in alphabetical order.
#'
#' @returns Nothing, but saves the data to the Azure blob
#'
#' @export
update_coverage <- function(indicator_id, iso3) {
  df <- data.frame(
    iso3 = sort(unique(iso3))
  )

  cs$update_az_file(
    df = df,
    name = paste0("output/", indicator_id, "/coverage.parquet")
  )
}
