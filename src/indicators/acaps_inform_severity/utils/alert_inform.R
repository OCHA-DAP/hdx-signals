box::use(
  dplyr,
  zoo
)

#' Alert ACAPS INFORM Severity data
#'
#'We compute the maximum value in the previous year and in the previous 3 years.
# If the current value is higher we create an alert defined with two degrees of
# severity. Level one if it's the highest value in the last year, level 2 if it's
# the highest in the last 3 years. The INFORM index should be at least 3 to alert
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
      ),
      diff_num_crises = count - dplyr$lag(count)

    ) |>
    dplyr$filter(
      inform_severity_index>=3
    ) |>
    dplyr$filter(
     higher_1yr | (diff_num_crises>0 & count>2)
    ) |>
    dplyr$transmute(
      iso3,
      indicator_name = "inform_severity",
      indicator_source = "acaps",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      date = as.Date(date),
      value = as.numeric(inform_severity_index),
      alert_level_numeric = as.integer(dplyr$case_when(
        is.na(higher_3yr) ~ 1,
        higher_3yr ~ 2,
        higher_1yr & (diff_num_crises>0) ~ 2,
        .default = 1
      ))
    )
}
