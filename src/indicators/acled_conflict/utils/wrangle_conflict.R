box::use(dplyr)
box::use(tidyr)
box::use(lubridate)
box::use(zoo)

box::use(cs = ../../../../src/utils/cloud_storage)
box::use(../../../../src/utils/country_codes)

#' Wrangle conflict data
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
#' @param df_raw Raw conflict data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw, first_run = FALSE) {
  df_info <- cs$read_az_file("input/acled_info.parquet")

  # lowest possible start_date based on when data was downloaded
  start_date_min <- if (first_run) as.Date("1500-01-01") else Sys.Date() - lubridate$days(1500)

  df_raw |>
    dplyr$mutate(
      iso3 = country_codes$ison_to_iso3(iso)
    ) |>
    dplyr$group_by(
      iso3, date = event_date
    ) |>
    dplyr$summarize(
      fatalities = sum(fatalities),
      notes = paste(notes, collapse = " "),
      .groups = "drop"
    ) |>
    dplyr$left_join(
      df_info,
      by = "iso3"
    ) |>
    dplyr$group_by(
      iso3, acled_hdx_url
    ) |>
    tidyr$complete( # completes data between start_date from ACLED report and max date
      date = seq.Date(min(min(date), max(start_date, start_date_min)), max(date), by = "day"),
      fill = list(fatalities = 0)
    ) |>
    dplyr$mutate(
      fatalities_7d = zoo$rollsumr(fatalities, k = 7, fill = NA),
      fatalities_30d = zoo$rollsumr(fatalities, k = 30, fill = NA)
    ) |>
    dplyr$ungroup()
}
