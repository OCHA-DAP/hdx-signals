box::use(
  idmc,
  dplyr,
  zoo
)

box::use(
  cs = src/utils/cloud_storage
)

#' Wrangle displacement data
#'
#' Takes in the IDMC displacement data and turns it into the proper format for generating
#' alerts.
#'
#' * Creates a daily time series from the events, splitting between conflict-related
#'     displacement and other types of displacement.
#' * Generates 7 day and 30 day rolling sums for plotting and alerting respectively.
#'
#' If the dataset is on conflict, the IDMC IDU currently has issues where only
#' very recent years, maybe 2022 or 2023 onward, has reliable data for certain countries.
#' To fix this, we currently filter the data based on start dates stored in
#' `input/idmc_dates.parquet`.
#'
#' @param df_raw Raw conflict data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    dplyr$group_by(
      displacement_type, iso3
    ) |>
    idmc$idmc_transform_daily() |>
    dplyr$left_join(
      y = cs$read_az_file("input/idmc_dates.parquet"),
      by = c("iso3", "displacement_type")
    ) |>
    dplyr$filter(
      is.na(start_date) | date >= start_date # filter to consistent time series
    ) |>
    dplyr$group_by(
      displacement_type, iso3
    ) |>
    dplyr$mutate(
      displacement_7d = zoo$rollsumr(displacement_daily, k = 7, fill = NA),
      displacement_30d = zoo$rollsumr(displacement_daily, k = 30, fill = NA)
    ) |>
    dplyr$ungroup() |>
    dplyr$select(
      iso3,
      displacement_type,
      date,
      displacement_daily,
      displacement_7d,
      displacement_30d
    )
}
