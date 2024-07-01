box::use(
  dplyr,
  tidyr,
  utils
)

#' Creates cholera alerts dataset
#'
#' Creates base alert dataset for cholera. Alerts are generated when
#' cases cross 1,000 or 5,000 from below.
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
#'
#' @export
alert <- function(df_wrangled) {
  df_wrangled |>
    dplyr$group_by(iso3) |>
    dplyr$mutate(
      `1` = lim_alert(cholera_cases, 1000),
      `2` = lim_alert(cholera_cases, 5000),
    ) |>
    dplyr$ungroup() |>
    tidyr$pivot_longer(
      dplyr$matches("^[1-2]$"),
      names_to = "alert_level_numeric",
      names_transform = as.integer
    ) |>
    dplyr$filter(
      value
    ) |>
    dplyr$group_by(
      iso3,
      start_date
    ) |>
    dplyr$summarize(
      date = max(date),
      alert_level_numeric = utils$tail(alert_level_numeric, n = 1),
      value = utils$tail(cholera_cases, n = 1),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      indicator_name = "cholera",
      indicator_source = "who",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      .after = iso3
    ) |>
    dplyr$select(
      -start_date
    )
}

#' Alert when limit crossed from below
#'
#' @param x Vector
#' @param lim Limit
#'
#' @returns Boolean vector
lim_alert <- function(x, lim) {
  x >= lim & dplyr$lag(x) < lim
}
