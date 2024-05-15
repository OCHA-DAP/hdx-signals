box::use(dplyr)
box::use(readr)

#' Filters wrangled data to find increase in biggest phase group
#'
#' For the IPC, we want to use as the headline the highest group that has
#' an increase. For instance, if there is an increase in P3+ and P4+, we will
#' use the P4+ as our key title at the top of the plot and the `value` in the
#' alerts data frame.
#'
#' Used in `alert_food_insecurity.R` and `plot_food_insecurity.R`.
#'
#' @param df Data frame of wrangled IPC data
#'
#' @returns Filtered data frame with phase level
#'
#' @export
ipc_alert_filter <- function(df) {
  df |>
    dplyr$filter(
      phase %in% c("p3plus", "p4plus", "phase5")
    ) |>
    dplyr$filter(
      `percentage-current` > `percentage-current_lag` |
        `percentage-current` < `percentage-projected` |
        `percentage-current` < `percentage-second_projected`
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
    dplyr$ungroup()
}
