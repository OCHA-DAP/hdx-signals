box::use(
  dplyr,
  janitor
)

box::use(
  src/signals/filter_alerts,
  src/signals/template_data,
  src/utils/add_locations_metadata
)

#' Generate and upload alerts data frame
#'
#' Generates alerts data frame. Takes in data frame of alerts from any
#' indicator and does the following steps:
#'
#' - Checks that the alerts data frame on Azure has no rows. These must be triaged
#'     into campaigns prior to generating any new alerts.
#' - Validates necessary columns are present, of the correct type, and in correct order.
#' - Adds `alert_level` to data, converting from `alert_level_numeric`.
#' - Creates `location` and `region` name columns from ISO3.
#' - Filters alerts.
#' - Uploads the alerts to Azure.
#'
#' If `HS_FIRST_RUN` is `TRUE`, the alerts are filtered differently to be
#' spaced out as if they were generated through monitoring. If `HS_DRY_RUN`
#' is `TRUE`, then the alerts are filtered to be the latest alert available
#' for the locations, ignoring if there are other recent alerts. This ensures
#' there are alerts available for testing.
#'
#' @param df Data frame of indicator alerts.
#' @param indicator_id ID of the indicator
#'
#' @returns Nothing, alerts are uploaded to Azure
#'
#' @export
generate_alerts <- function(df, indicator_id) {
  df |>
    validate_alerts() |>
    add_alert_level() |>
    add_locations_metadata$add_locations_metadata() |>
    filter_alerts$filter_alerts(
      indicator_id = indicator_id
    )
}

#' Validates that alerts have the correct names and typing when passed in.
#'
#' Checks that columns are present and have correct typing. Location column is
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
