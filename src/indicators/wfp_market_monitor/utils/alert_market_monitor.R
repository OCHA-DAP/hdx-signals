box::use(dplyr)

#' Creates displacement alerts dataset
#'
#' Creates base alert dataset for displacement. Uses `alert_daily_ts()` twice
#' to create alerts for when the data is
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
#'
#' @export
alert <- function(df_wrangled) {
  df_wrangled |>
    dplyr$mutate(
      alert_level_numeric = dplyr$case_when(
        basket_change_class == "High" ~ 1,
        basket_change_class == "Severe" ~ 2,
        .default = 0,
        .ptype = integer()
      )
    ) |>
    dplyr$filter(
      !is.na(basket_change),
      alert_level_numeric > 0
    ) |>
    dplyr$transmute(
      iso3,
      indicator_name = "market_monitor",
      indicator_source = "wfp",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      date,
      value = basket_change,
      alert_level_numeric
    )
}
