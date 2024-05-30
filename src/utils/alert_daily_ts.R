box::use(dplyr)
box::use(tidyr)
box::use(janitor)
box::use(zoo)
box::use(rlang[`!!`])

#' Creates the alert data frame
#'
#' Creates alerts from a data frame of daily time series data. Requires that
#' `iso3`, `date`, and `value` columns are passed in with the correct type.
#' As well, it checks that the `date` column is consecutive days.
#'
#' @param df Data frame of alerts with `iso3`, `date`, and `value` columns.
#' @param min_val Minimum value for the necessary alert.
#' @param rs_days Number of days for the rolling sum, defaults to `30`.
#'
#' @returns Alerts data frame with columns `iso3`, `date`, `value`, and
#'     `alert_level_numeric`.
#'
#' @export
alert_daily_ts <- function(df, val_col, min_val, rs_days = 30) {
  validate_ts_df(df, val_col)
  df |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$mutate(
      `1` = .data[[val_col]] >= zoo$rollmaxr(.data[[val_col]], k = 366, fill = NA) & .data[[val_col]] >= min_val,
      `2` = .data[[val_col]] >= zoo$rollmaxr(.data[[val_col]], k = 1096, fill = NA) & .data[[val_col]] >= min_val
    ) |>
    tidyr$pivot_longer(
      cols = dplyr$matches("^[1-2]$"),
      names_to = "alert_level_numeric",
      values_to = "alert_bool",
      names_transform = as.integer
    ) |>
    dplyr$filter(
      alert_bool
    ) |>
    dplyr$select(
      iso3, date, value = !!val_col, alert_level_numeric
    ) |>
    dplyr$group_by(
      iso3, date
    ) |>
    dplyr$filter( # ensure we take the highest alert level each time we would generate one
      alert_level_numeric == max(alert_level_numeric, -Inf)
    ) |>
    dplyr$ungroup()
}

#' Validate data frame is ready for `alert_ts()`.
#'
#' Checks that necessary columns are present in the dataset, that they are of the
#' correct type, and that the date is properly organized. These are `iso3`,
#' `date`, and `value`, which should be `character`, `Date`, and `numeric` respectively.
#' The data is grouped by `iso3` and then
#' checks that there are no missing dates.
#'
#' @param df Data frame of wrangled data to alert on, with `iso3`, `date`, and
#'     `value` columns. It should already be sorted/arranged, otherwise errors
#'     will be generated.
#'
#' @returns Nothing, just throws errors if input `df` is incorrect.
validate_ts_df <- function(df, val_col) {
  df_check <- dplyr$tibble(
    iso3 = character(),
    date = as.Date(integer(), origin = "1970-01-01"),
    "{val_col}" := numeric()
  )

  col_names <- names(df_check)

  if (!all(col_names %in% names(df))) {
    stop(
      "`iso3`, `date`, and `",
      val_col,
      "` are required columns in `df` for using `alert_daily_ts()`.",
      call. = FALSE
    )
  }

  if (!janitor$compare_df_cols_same(df_check, df[, col_names], bind_method = "rbind")) {
    stop(
      "Column types are not of the required type for `alert_daily_ts()`.",
      call. = FALSE
    )
  }

  # throw an error if any of the dates not consecutive within a location
  date_check <- df |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$mutate(
      date_check = c(1, diff(date)) == 1
    ) |>
    dplyr$pull(date_check)
  if (any(!date_check)) {
    stop(
      "Dates are not completely consecutive in the `df` for using `alert_daily_ts().",
      " Please fix the issue and try again.",
      call. = FALSE
    )
  }

}
