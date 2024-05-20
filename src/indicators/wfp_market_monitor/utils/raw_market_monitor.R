box::use(cs = ../../../../src/utils/cloud_storage)

#' Download raw market monitoring data
#'
#' Downloads raw market monitoring data from Azure database. Is uploaded directly
#'
#' @export
raw <- function() {
  cs$read_az_file(
    name = "BasketCostChange.csv",
    blob = "wfp"
  )
}
