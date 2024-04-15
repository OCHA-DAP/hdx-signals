box::use(dplyr)

box::use(cs = ../utils/cloud_storage)

#' Filters alerts only to new alerts
#'
#' *Ongoing alerts*
#'
#' For ongoing alert monitoring, we only want to generate new alerts that have
#' not seen recent alerts. The logic is that alerts are only generated:
#'
#' - If they have been generated in the past 90 days compared to `Sys.date()`
#' - If an alert of equal or higher level has not been seen in the past 180 days
#'
#' If there are multiple alerts within the past 90 days, the logic used in the
#' code always takes first the **highest priority** alerts and then the
#' **latest**.
#'
#' *First run*
#'
#' When it's the first run for any alert, we generate all historic alerts, even
#' if not in the past 90 days. Since we are not comparing it to an existing
#' campaigns file, recursive filtering is used to ensure that there are 180 days
#' between alerts.
#'
#' An error is generated if the campaigns file is found on the Azure Data Store.
#' This is to ensure that no accidental re-creation of campaign data is done until
#' campaigns are explicitly removed.
#'
#' @param df_alerts Data frame of alerts to be filtered
#' @param fn_df_campaigns File name of the campaigns data frame in cloud storage
#' @param first_run Whether or not this is the first run of the campaign.
#'
#' @returns Data frame of new alerts matching the criteria above
#'
#' @export
filter_alerts <- function(df_alerts, fn_df_campaigns, first_run = FALSE) {
  # no need to do anything for empty data frame
  if (nrow(df_alerts) == 0) {
    return(df_alerts)
  }

  if (!first_run) {
    filter_alerts_ongoing(df_alerts, fn_df_campaigns)
  } else {
    filter_alerts_first_run(df_alerts, fn_df_campaigns)
  }
}

#' Filter alerts for ongoing monitoring
#'
#' Used to filter alerts to find new ones, when a campaigns file already exists.
#' Finds alerts in the past 90 days that have not seen previous alerts in the
#' past 180 days.
#'
#' Comparisons are done against the existing campaigns data because for some
#' datasets, historic data shifts. The most reliable way to understand what
#' campaigns have been sent out is to directly query our campaigns data to ensure
#' we are not sending out alerts within 180 days.
filter_alerts_ongoing <- function(df_alerts, fn_df_campaigns) {
  # get recent campaigns
  df_campaigns <- cs$read_az_file(fn_df_campaigns) |>
    dplyr$filter(
      Sys.Date() - campaign_date <= 180
    )

  # first we get the highest priority and latest alerts from the new data frame
  # that are found in the past 3 months
  df_new_alerts <- df_alerts |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$filter(
      Sys.Date() - date <= 90
    ) |>
    dplyr$filter(
      alert_level == "High concern" | all(alert_level == "Medium concern")
    ) |>
    dplyr$filter(
      date == max(date, as.Date("1500-01-01"))
    ) |>
    dplyr$ungroup() |>
    dplyr$arrange(
      country
    )

  # drop new high alerts only if another high alert in the past 6 months
  df_new_alerts_high <- df_new_alerts |>
    dplyr$filter(
      alert_level == "High concern"
    ) |>
    dplyr$anti_join(
      dplyr$filter(df_campaigns, alert_level == "High concern"),
      by = "iso3"
    )

  # drop new medium alerts if any alerts in the past 6 months
  df_new_alerts_medium <- df_new_alerts |>
    dplyr$filter(
      alert_level == "Medium concern"
    ) |>
    dplyr$anti_join(
      df_campaigns,
      by = "iso3"
    )

  dplyr$bind_rows(
    df_new_alerts_high,
    df_new_alerts_medium
  )
}

#' Filter alerts for the first run
#'
#' Used to filter alerts for the first run, when no campaign file exists. Uses
#' recursive filtering to remove alerts within 180 days of each other. Generates
#' an error if a campaign file exists.
filter_alerts_first_run <- function(df_alerts, fn_df_campaigns) {
  if (fn_df_campaigns %in% cs$az_file_detect()) {
    stop(
      "The campaign file '",
      fn_df_campaigns,
      "' is found on Azure. Cannot do the first campaign run if a campaign",
      "file exists. Think very carefully before just deleting an existing ",
      "campaign dataset to re-run the first run.",
      call. = FALSE
    )
  }

  # recursively filter all of our alerts for each country
  df_alerts |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$group_modify(
      .f = \(df, y) recursive_filter_country(df)
    ) |>
    dplyr$ungroup()
}

#' Recursively filters country alerts
#'
#' Starting with the oldest alerts, recursively
recursive_filter_country <- function(df) {
  # first make sure if anything in the past 90 days, so will generate a
  # campaign with an email, that we keep the highest concern alert
  df <- dplyr$arrange(df, date)

  i <- 1
  while(i < nrow(df)) {
    current_row <- df[i,]
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
