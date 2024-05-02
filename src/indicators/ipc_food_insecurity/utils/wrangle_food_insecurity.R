box::use(dplyr)
box::use(tidyr)
box::use(stringr)

#' Wrangle food insecurity data
#'
#' Takes in the ACLED data and turns it into the proper format for generating
#' alerts.
#'
#' * Creates an `iso3` column.
#' * Fills in all dates from the beginning of
#'     coverage (derived from
#'     https://acleddata.com/acleddatanew/wp-content/uploads/dlm_uploads/2019/01/ACLED_Country-and-Time-Period-coverage_updatedFeb2022.pdf)
#'     and updated in `src-static/update_acled_start.R`. If not `first_run`,
#'     only fills in to the start date of when the data was downloaded.
#' * Summarize all country-date dyads to get sum of fatalities and paste together
#'     all notes information.
#'
#' @param df_raw Raw food insecurity data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$select(
      iso3,
      date,
      analysis_id,
      plot_date = analysis_period_start,
      period,
      dplyr$matches("(phase[3-5]|plus)_percentage")
    ) |>
    dplyr$arrange(
      date,
      .by_group = TRUE
    ) |>
    tidyr$pivot_longer(
      cols = ends_with("percentage"),
      names_to = "phase",
      names_transform = \(x) stringr$str_remove(x, "_percentage"),
      values_to = "percentage"
    ) |>
    tidyr$pivot_wider(
      names_from = period,
      values_from = c(percentage, plot_date),
      names_sep = "-"
    ) |>
    dplyr$group_by(
      iso3, phase
    ) |>
    dplyr$mutate(
      `percentage-current_lag` = dplyr$lag(`percentage-current`)
    ) |>
    dplyr$ungroup()
}
