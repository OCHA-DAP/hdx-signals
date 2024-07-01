box::use(
  dplyr,
  tidyr,
  zoo
)

box::use(
  cs = src/utils/cloud_storage,
  src/utils/location_codes
)

#' Wrangle conflict data
#'
#' Takes in the ACLED data and turns it into the proper format for generating
#' alerts.
#'
#' * Creates an `iso3` column.
#' * Fills in all dates from the beginning of coverage
#'     (updated in `src-static/update_acled_start.R`). If not `first_run`,
#'     only fills in to the start date of when the data was downloaded.
#' * Summarize all location-date dyads to get sum of fatalities and paste together
#'     all notes information.
#'
#' @param df_raw Raw conflict data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw, first_run = FALSE) {
  df_info <- cs$read_az_file("input/acled_info.parquet")

  df_raw |>
    dplyr$mutate(
      iso3 = location_codes$ison_to_iso3(as.numeric(iso)),
      fatalities = as.numeric(fatalities)
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
      iso3,
      acled_hdx_url
    ) |>
    tidyr$complete( # completes data between start_date from ACLED report and max date
      date = seq.Date(
        from = min(min(date), max(start_date, as.Date("2018-01-01"))),
        to = max(date),
        by = "day"
      ),
      fill = list(fatalities = 0)
    ) |>
    dplyr$mutate(
      fatalities_7d = zoo$rollsumr(fatalities, k = 7, fill = NA),
      fatalities_30d = zoo$rollsumr(fatalities, k = 30, fill = NA)
    ) |>
    dplyr$ungroup()
}
