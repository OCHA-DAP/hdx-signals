box::use(dplyr)
box::use(readr)

box::use(./util_alert_filter)

#' Creates food insecurity alerts dataset
#'
#' Alerts are generated whenever the current estimate is higher than the previous
#' current estimate, or either projection is higher than current in the analysis.
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
alert <- function(df_wrangled) {
  df_wrangled |>
    util_alert_filter$ipc_alert_filter() |>
    dplyr$transmute(
      iso3,
      indicator_name = "food_insecurity",
      indicator_source = "ipc",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      date,
      alert_level_numeric = as.integer(pmin(phase_level - 2, 2)),
      value = pmax(`percentage-current`, `percentage-projected`, `percentage-second_projected`, na.rm = TRUE)
    )
}
