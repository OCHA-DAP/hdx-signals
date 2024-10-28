box::use(
  dplyr,
  zoo
)

#' Alert ACAPS INFORM Severity data
#'
#' Currently does nothing as no need for additional wrangling.
#'
#' @export
alert <- function(df_wrangled) {
  df_wrangled |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$mutate(
      lag_inform = dplyr$lag(inform_severity_index),
      inform_1yr = zoo$rollmaxr(x = lag_inform, k = 12, fill = NA),
      inform_3yr = zoo$rollmaxr(x = lag_inform, k = 36, fill = NA),
      higher_1yr = inform_severity_index > inform_1yr,
      higher_3yr = inform_severity_index > inform_3yr,
      higher = dplyr$case_when(
        higher_3yr ~ "3 year",
        higher_1yr ~ "1 year"
      )
    ) |>
    dplyr$filter(
      higher_1yr
    ) |>
    dplyr$transmute(
      iso3,
      indicator_name = "inform_severity",
      indicator_source = "acaps",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      date,
      value = inform_severity_index,
      alert_level_numeric = dplyr$case_when(
        is.na(higher_3yr) ~ 1,
        higher_3yr ~ 2,
        .default = 1
      )
    )
}
