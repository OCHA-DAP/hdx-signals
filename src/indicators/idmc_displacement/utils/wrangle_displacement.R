box::use(idmc)
box::use(dplyr)
box::use(zoo)

#' Wrangle displacement data
#'
#' Takes in the IDMC displacement data and turns it into the proper format for generating
#' alerts.
#'
#' * Creates a daily time series from the events, splitting between conflict-related
#'     displacement and other types of displacement.
#' * Generates 7 day and 30 day rolling sums for plotting and alerting respectively.
#'
#' @param df_raw Raw conflict data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    dplyr$mutate(
      displacement_type = ifelse(
        displacement_type == "Conflict",
        "Conflict",
        "Disaster"
      )
    ) |>
    dplyr$group_by(
      displacement_type, iso3
    ) |>
    idmc$idmc_transform_daily() |>
    dplyr$group_by(
      displacement_type, iso3
    ) |>
    dplyr$mutate(
      displacement_7d = zoo$rollsumr(displacement_daily, k = 7, fill = NA),
      displacement_30d = zoo$rollsumr(displacement_daily, k = 30, fill = NA)
    ) |>
    dplyr$select(
      iso3,
      displacement_type,
      date,
      displacement_daily,
      displacement_7d,
      displacement_30d
    ) |>
    dplyr$ungroup()
}
