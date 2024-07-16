box::use(cs = ../../../../src/utils/cloud_storage)

#' Download raw market monitoring data
#'
#' Downloads raw market monitoring data from Azure database. Is uploaded directly
#'
#' @export
raw <- function() {
  df <- cs$read_az_file(
    name = "BasketCostChange.csv",
    container = "wfp"
  )

  req_cols <- c(
    "MMFPSNTotImpactMonthlyChange",
    "MMFPSNTotImpactMonthlyCode",
    "MMFPSNDate",
    "CountryName"
  )

  if (!all(req_cols %in% names(df))) {
    stop(
      "Not all required columns available in the raw WFP data. Check the ",
      "data provided by WFP to see how it has changed and follow up accordingly.",
      call. = FALSE
    )
  }
  df
}
