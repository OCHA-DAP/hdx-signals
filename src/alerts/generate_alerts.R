box::use(purrr)
box::use(countrycode)
box::use(dplyr)
box::use(janitor)

box::use(./filter_alerts)
box::use(./template_data)
box::use(cs = ../utils/cloud_storage)
box::use(../utils/add_country_info[add_country_info])

#' Generate and upload alerts data frame
#'
#' Generates alerts data frame. Takes in data frame of alerts from any
#' indicator and does the following steps:
#'
#' - Checks that the alerts data frame on Azure has no rows. These must be triaged
#'     into campaigns prior to generating any new alerts.
#' - Validates necessary columns are present, of the correct type, and in correct order.
#' - Adds `alert_level` to data, converting from `alert_level_numeric`.
#' - Creates `country` and `region` name columns from ISO3.
#' - Filters alerts.
#' - Uploads the alerts to Azure.
#'
#' @param df Data frame of indicator alerts.
#' @param indicator_id ID of the indicator
#' @param first_run Whether or not this is the first run of an indicator. Used
#'     to determine different filtering methods.
#' @param dry_run Whether or not we are generating a dry run, in which case we
#'     filter the alerts to the latest, ignoring if there have been recent alerts.
#'
#' @returns Nothing, alerts are uploaded to Azure
#'
#' @export
generate_alerts <- function(df, indicator_id, first_run = FALSE, dry_run = FALSE) {
  df |>
    validate_alerts() |>
    add_alert_level() |>
    add_country_info() |>
    filter_alerts$filter_alerts(
      indicator_id = indicator_id,
      first_run = first_run,
      dry_run = dry_run
    )
}

#' Validates that alerts have the correct names and typing when passed in.
#'
#' Checks that columns are present and have correct typing. Country column is
#' not checked for because it is created in `generate_alerts()` above.
#'
#' @param df Data frame of alerts to validate
validate_alerts <- function(df) {
  df_check <- template_data$alerts_template_base

  # only check that required columns are there, don't filter them out
  if (!janitor$compare_df_cols_same(df[, names(df_check)], df_check, bind_method = "rbind")) {
    stop(
      "Alerts data frame does not have the correct columns and typing.",
      call. = FALSE
    )
  }

  df
}

#' Add alert level
#'
#' Add the `alert_level` column to the data frame.
add_alert_level <- function(df) {
  df <- dplyr$mutate(
    df,
    alert_level = dplyr$case_when(
      alert_level_numeric == 1 ~ "Medium concern",
      alert_level_numeric == 2 ~ "High concern"
    ),
    .after = alert_level_numeric
  )

  if (any(is.na(df$alert_level))) {
    stop(
      "Missing values when calculating `alert_level`. Check that ",
      "`alert_level_numeric` has been calculated correctly.",
      call. = FALSE
    )
  }

  df
}
