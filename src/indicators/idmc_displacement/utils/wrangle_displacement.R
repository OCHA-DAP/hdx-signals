box::use(
  idmc,
  dplyr,
  zoo
)

box::use(
  src/utils/add_locations_metadata
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
#' To fix this, we currently filter the data for any HRP country to drop the beginning
#' of the time series if there is no reported displacement. This should be removed
#' once the IDMC is able to fix their system to match more what ACLED is able to
#' provide.
#'
#' @param df_raw Raw conflict data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_wrangled <- df_raw |>
    dplyr$filter(
      displacement_type %in% c("Conflict", "Disaster") # other is no longer used by IDMC
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

  # temporary filtering to fix IDMC issues
  if (unique(df_wrangled$displacement_type) == "Conflict") {
    df_wrangled <- df_wrangled |>
      add_locations_metadata$add_locations_metadata() |>
      dplyr$group_by(
        iso3
      ) |>
      dplyr$filter(
        cumsum(dplyr$displacement_daily) > 0 | !hrp # only do the filtering for HRP countries
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
}
