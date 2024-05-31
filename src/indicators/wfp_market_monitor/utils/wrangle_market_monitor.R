box::use(dplyr)

box::use(../../../utils/country_codes)

#' Download raw market monitoring data
#'
#' Downloads raw market monitoring data from Azure database. Is uploaded directly
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    dplyr$filter(
      MMFPSNDataLevel == "National"
    ) |>
    dplyr$transmute(
      iso3 = country_codes$names_to_iso3(CountryName),
      date = MMFPSNDate,
      basket_change = MMFPSNTotImpactMonthlyChange,
      basket_change_class = MMFPSNTotImpactMonthlyCode
    ) |>
    dplyr$arrange(
      iso3,
      date
    )
}
