box::use(dplyr)
box::use(readr)

#' Creates agricultural hotspots alerts dataset
#'
#' Alerts are generated whenever an agricultural hotspots is declared, either hotspot (1)
#' or major hotspot (2). The alerts happen when moving from one state to another.
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
alert <- function(df_wrangled) {
  df_wrangled |>
    dplyr$transmute(
      iso3,
      indicator_name = "agricultural_hotspots",
      indicator_source = "jrc",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      date,
      alert_level_numeric = as.integer(hs_code),
      value = hs_code,
      date_label,
      comment
    ) |>
    dplyr$group_by(iso3) |>
    dplyr$filter(
      alert_level_numeric > dplyr$lag(alert_level_numeric)
    ) |>
    dplyr$ungroup()
}
