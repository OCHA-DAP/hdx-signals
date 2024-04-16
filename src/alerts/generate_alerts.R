box::use(purrr)
box::use(countrycode)
box::use(dplyr)
box::use(janitor)

box::use(./filter_alerts)
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
#' @param fn_df_alerts Name of the alerts data frame on Azure
#' @param fn_df_campaign Name of the campaigns data frame on Azure
#' @param first_run Whether or not this is the first run of an indicator. Used
#'     to determine different filtering methods.
#'
#' @returns Nothing, alerts are uploaded to Azure
#'
#' @export
generate_alerts <- function(df, fn_df_alerts, fn_df_campaigns, first_run = FALSE) {
  check_existing_alerts(fn_df_alerts)

  df_alerts <- df |>
    validate_alerts() |>
    add_alert_level() |>
    filter_alerts$filter_alerts(
      fn_df_campaigns = fn_df_campaigns,
      first_run = first_run
    ) |>
    add_country_info()

  cs$update_az_file(df_alerts, fn_df_alerts)
  df_alerts
}

#' Check existing alerts
#'
#' Checks that the alerts data frame on Azure is empty, if it exists.
#' If not empty, then these still need to be triaged into campaigns before
#' generating any new alerts.
#'
#' @param fn_df_alerts
check_existing_alerts <- function(fn_df_alerts) {
  if (fn_df_alerts %in% cs$az_file_detect()) {
    df <- cs$read_az_file(fn_df_alerts)
    if (nrow(df) != 0) {
      stop(
        "The alerts data ",
        fn_df_alerts,
        " on Azure is non-empty. Please triage those alerts prior ",
        "to generating new alerts.",
        call. = FALSE
      )
    }
  }
}

#' Validates that alerts have the correct names and typing when passed in.
#'
#' Checks that columns are present and have correct typing. Country column is
#' not checked for because it is created in `generate_alerts()` above.
#'
#' @param df Data frame of alerts to validate
validate_alerts <- function(df) {
  df_check <- dplyr$tibble(
    iso3 = NA_character_,
    indicator_name = NA_character_,
    indicator_source = NA_character_,
    date = as.Date(integer(0), origin = "1970-01-01"),
    alert_level_numeric = NA_integer_,
    value = NA_real_
  )

  if (!janitor$compare_df_cols_same(df, df_check, bind_method = "rbind")) {
    stop(
      "Alerts data frame does not have the correct columns and typing.",
      call. = FALSE
    )
  }

  df[,names(df_check)]
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
