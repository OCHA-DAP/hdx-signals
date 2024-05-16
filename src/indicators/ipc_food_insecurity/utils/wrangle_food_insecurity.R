box::use(dplyr)
box::use(tidyr)
box::use(stringr)
box::use(readr)

#' Wrangle food insecurity data
#'
#' FILL IN
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
    dplyr$mutate(
      from = ifelse(
        readr$parse_number(from) == readr$parse_number(to), # identifying same year
        stringr$str_remove(from, " [\\d]+"),
        from
      ),
      map_date = paste(from, to, sep = " to ")
    ) |>
    dplyr$select(
      iso3,
      date,
      analysis_id,
      plot_date = analysis_period_start,
      map_date,
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
      values_from = c(percentage, plot_date, map_date),
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
