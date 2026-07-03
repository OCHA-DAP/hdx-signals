box::use(
  dplyr,
  scales
)

#' Creates SEA5 anomaly alerts dataset
#'
#' Generates alerts when `p_5rp` exceeds 20%, indicating a high probability
#' of an extreme precipitation anomaly (above the 5-year return period).
#' Alerts at level 1 when p_5rp > 20%.
#'
#' @param df_wrangled Wrangled data frame
#'
#' @returns Alerts dataset
#'
#' @export
alert <- function(df_wrangled) {
  df_wrangled |>
    dplyr$filter(p_5rp > 0.2) |>
    dplyr$mutate(
      alert_level_numeric = 1L,
      value = p_5rp,
      indicator_name = "anomaly",
      indicator_source = "sea5",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      .after = iso3
    ) |>
    dplyr$mutate(
      title = paste0(
        scales$label_percent(accuracy = 1)(value),
        " probability of exceeding the 5-year return period"
      )
    )
}
