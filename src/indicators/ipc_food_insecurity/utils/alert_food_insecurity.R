box::use(dplyr)
box::use(readr)

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
    dplyr$filter(
      phase %in% c("p3plus", "p4plus", "phase5")
    ) |>
    dplyr$filter(
      `percentage-current` > `percentage-current_lag` | `percentage-current` < `percentage-projected` | `percentage-current` < `percentage-second_projected`
    ) |>
    dplyr$mutate(
      phase_level = readr$parse_number(phase)
    ) |>
    dplyr$group_by(
      iso3, date
    ) |>
    dplyr$filter(
      phase_level == max(phase_level)
    ) |>
    dplyr$ungroup() |>
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