box::use(dplyr)
box::use(rlang[`!!`])

box::use(cs = ../utils/cloud_storage)
box::use(../utils/hs_dry_run)
box::use(../utils/hs_first_run)

#' Filters alerts only to new alerts
#'
#' *Ongoing alerts*
#'
#' For ongoing alert monitoring, we only want to generate new alerts that have
#' not seen recent alerts. The logic is that alerts are only generated:
#'
#' - If they have been generated in the past 60 days compared to `Sys.date()`
#' - If an alert of equal or higher level has not been seen in the past 180 days
#'
#' If there are multiple alerts within the past 60 days, the logic used in the
#' code always takes first the **highest priority** alerts and then the
#' **latest**.
#'
#' *First run*
#'
#' When it's the first run for any alert, we generate all historic alerts, even
#' if not in the past 60 days. Since we are not comparing it to an existing
#' campaigns file, recursive filtering is used to ensure that there are 180 days
#' between alerts.
#'
#' An error is generated if the campaigns file is found on the Azure Data Store.
#' This is to ensure that no accidental re-creation of campaign data is done until
#' campaigns are explicitly removed.
#'
#' If `HS_FIRST_RUN` is `TRUE`, then first run is generated. You can do a dry run
#' of the first run if you set `HS_DRY_RUN` to `TRUE`. If `HS_DRY_RUN` is `TRUE`
#' and `HS_FIRST_RUN` is `FALSE`, then it does a monitoring dry run. Otherwise,
#' it follows the monitoring ongoing alerts logic.
#'
#' @param df_alerts Data frame of alerts to be filtered.
#' @param indicator_id ID of the indicator, matching the first column
#'      in `input/indicator_mapping.parquet`.
#'
#' @returns Data frame of new alerts matching the criteria above
#'
#' @export
filter_alerts <- function(df_alerts, indicator_id) {
  # no need to do anything for empty data frame
  if (nrow(df_alerts) == 0) {
    return(df_alerts)
  }

  if (hs_first_run$hs_first_run()) {
    filter_alerts_first_run(df_alerts)
  } else if (hs_dry_run$hs_dry_run()) {
    filter_alerts_test(df_alerts)
  } else {
    filter_alerts_ongoing(df_alerts, indicator_id)
  }
}

#' Filter alerts for ongoing monitoring
#'
#' Used to filter alerts to find new ones, when a campaigns file already exists.
#' Finds alerts in the past 60 days that have not seen previous alerts in the
#' past 180 days.
#'
#' Comparisons are done against the existing campaigns data because for some
#' datasets, historic data shifts. The most reliable way to understand what
#' campaigns have been sent out is to directly query our campaigns data to ensure
#' we are not sending out alerts within 180 days.
filter_alerts_ongoing <- function(df_alerts, indicator_id) {
  # we know that this file exists because it is checked in `check_existing_signals()`
  # so we get recent campaigns from it
  df_signals <- cs$read_az_file("output/signals.parquet") |>
    dplyr$filter(
      indicator_id == !!indicator_id,
      Sys.Date() - campaign_date <= 180
    )

  # first we get the highest priority and latest alerts from the new data frame
  # that are found in the past 2 months
  df_new_alerts <- df_alerts |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$filter(
      Sys.Date() - date <= 60
    ) |>
    dplyr$filter(
      alert_level == "High concern" | all(alert_level == "Medium concern")
    ) |>
    dplyr$filter(
      date == max(date, as.Date("1500-01-01"))
    ) |>
    dplyr$ungroup() |>
    dplyr$arrange(
      alert_level,
      location
    )

  # drop new high alerts only if another high alert in the past 6 months
  df_new_alerts_high <- df_new_alerts |>
    dplyr$filter(
      alert_level == "High concern"
    ) |>
    dplyr$anti_join(
      dplyr$filter(df_signals, alert_level == "High concern"),
      by = "iso3"
    )

  # drop new medium alerts if any alerts in the past 6 months
  df_new_alerts_medium <- df_new_alerts |>
    dplyr$filter(
      alert_level == "Medium concern"
    ) |>
    dplyr$anti_join(
      df_signals,
      by = "iso3"
    )

  dplyr$bind_rows(
    df_new_alerts_high,
    df_new_alerts_medium
  ) |>
    dplyr$arrange(
      location
    )
}

#' Filter alerts for the first run
#'
#' Used to filter alerts for the first run, when no campaign file exists. Uses
#' recursive filtering to remove alerts within 180 days of each other. Generates
#' an error if a campaign file exists.
filter_alerts_first_run <- function(df_alerts) {
  # recursively filter all of our alerts for each location
  df_alerts |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$group_modify(
      .f = \(df, y) recursive_subsequent_alerts(df)
    ) |>
    dplyr$ungroup()
}

#' Filters out location alerts
#'
#' Starting with the oldest alerts, filter out alerts that are within
#' 180 days of it, moving on to the next remaining alert.
recursive_subsequent_alerts <- function(df) {
  df <- dplyr$arrange(df, date)

  i <- 1
  while (i < nrow(df)) {
    current_row <- df[i, ]
    current_date <- current_row$date
    current_level <- current_row$alert_level_numeric

    df <- dplyr$filter(
      df,
      !(dplyr$between(as.numeric(date - current_date), 1, 180) & alert_level_numeric <= current_level)
    )
    i <- i + 1
  }
  df
}

#' Filters out alerts for testing
#'
#' Simply returns the latest alert for all locations in the data.
filter_alerts_test <- function(df) {
  df |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$filter(
      date == max(date, as.Date("1500-01-01"))
    ) |>
    dplyr$ungroup()
}
