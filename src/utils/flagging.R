box::use(stats)
box::use(dplyr)
box::use(zoo)
box::use(rlang[`:=`, .data, `!!`])
box::use(purrr)
box::use(tidyr)
box::use(stringr)
box::use(readr)
box::use(lubridate)

#' Calculate flags in a data frame
#'
#' Generates flags on `.data`. Works by automatically creating the rolling
#' sums needed for calculating the flags, calculating the flags, and then
#' assessing when an alert has been generated and at what level the alert
#' was generated.
#'
#' Works based on the groups in the data frame already passed in, such as `iso3`.
#'
#' @param .data Data frame
#' @param x_col `character` name of column to flag upon, which should be numeric.
#' @param first_since `numeric` vector indicating when to flag the first positive
#'     values. For instance, `180` would indicate flagging whenever there is a
#'     positive value after 180 days.
#' @param first_since_minimums `numeric` vector of minimum values such that if `x`
#'     is above it, no flags are generated. If a single value, the value is
#'     recycled. If the same length as `first_since`, the thresholds are matched
#'     one-to-one with `first_since`. Since `first_since` is concerned with the
#'     first value in a period of time, it only triggers on the first instance.
#'     The minimum is therefore not just on the single first day, but is measured
#'     against the following 60 days (which matches the period in which an email
#'     would be generated, older flags would not generate any email).
#' @param periods `numeric` vector of cumulation periods (in days) that flags
#'     are calculated, used
#'     for all of the `thresholds_...` arguments. For instance, `c(7, 30, 90)`
#'     would specify that flags are calculated for rolling sums of 7 days,
#'     30 days, and 90 days.
#' @param thresholds_pcts `numeric` vector of percentile to use for flagging,
#'     so that any time the value for `x` is over that percent quantile based
#'     on the historic data, a flag is
#'     generated. If a single value, the value is recycled. If the same length
#'     as `periods`, the thresholds are matched one-to-one with `periods`.
#' @param thresholds_static `numeric` vector of static thresholds to use for
#'     flagging, so that any time the value of `x` is above that value, a flag
#'     is generated. If a single value, the value is recycled. If the same length
#'     as `periods`, the thresholds are matched one-to-one with `periods`.
#' @param thresholds_minimums `numeric` vector of minimum values such that if `x`
#'     is above it, no flags are generated. If a single value, the value is
#'     recycled. If the same length as `periods`, the thresholds are matched
#'     one-to-one with `periods`.
#'
#' @returns Data frame with rolling sums and flags generated.
#'
#' @export
calculate_flags <- function(
    .data,
    x_col,
    first_since = NULL,
    first_since_minimums = NULL,
    periods = NULL,
    thresholds_pcts = NULL,
    thresholds_static = NULL,
    thresholds_minimums = NULL
) {
  # at least one of first_since and periods must be passed
  null_p <- is.null(periods)
  null_fs <- is.null(first_since)

  if (null_p && null_fs) {
    stop(
      "At least one of `first_since` and `periods` must be passed.",
      call. = FALSE
    )
  }

  # if periods specified, check that all arguments match as necessary

  if (!null_p) {
    # we order all of the arguments by periods
    # for later pmap
    p_order <- order(periods, decreasing = TRUE)
    periods <- periods[p_order]

    null_tp <- is.null(thresholds_pcts)
    null_ts <- is.null(thresholds_static)

    if (null_tp && null_ts) {
      stop(
        "One of `thresholds_pcts` or `thresholds_static` must be ",
        "passed to `generate_alerts()` if `periods` is ",
        "not NULL.",
        call. = FALSE
      )
    }

    # create thresholds that can't be met if null is set
    if (null_tp) {
      thresholds_pcts <- 1
    }

    if (null_ts) {
      thresholds_static <- Inf
    }

    # check the lengths of the inputs
    ln_p <- length(periods)
    ln_tp <- length(thresholds_pcts)
    ln_ts <- length(thresholds_static)


    if ((ln_p != ln_tp && ln_tp != 1) || (ln_p != ln_ts && ln_ts != 1)) {
      stop(
        "`thresholds_pcts` and `thresholds_static` must have the same length ",
        "as `periods` if passed to `generate_alerts()`, unless length 1, in ",
        " which case they are recycled.",
        call. = FALSE
      )
    }

    # reordering for later pmap
    # user has to pass in the correct order, and these keeps the order the same
    # since we reordered the percentiles
    if (ln_tp > 1) {
      thresholds_pcts <- thresholds_pcts[p_order]
    }

    if (ln_ts > 1) {
      # generate a warning if the static threshold don't monotonically increase
      # alongside `period`
      if (any(order(thresholds_static, decreasing = TRUE) < p_order)) {
        warning(
          "`thresholds_static` does not increase as `period` increases, which ",
          "may be in error. Ensure that the thresholds are passed correctly.",
          call. = FALSE
        )
      }

      thresholds_static <- thresholds_static[p_order]
    }

    if (is.null(thresholds_minimums)) {
      thresholds_minimums <- -Inf
    } else {
      ln_m <- length(thresholds_minimums)
      if (ln_p != ln_m && (ln_m != 1)) {
        stop(
          "`thresholds_minimum` must have the same length ",
          "as `periods` if passed to `generate_alerts()`, unless length 1, in ",
          " which case it is recycled.",
          call. = FALSE
        )
      } else if (ln_m > 1) {
        # generate a warning if the minimum threshold don't monotonically increase
        # alongside `period`
        if (any(order(thresholds_minimums, decreasing = TRUE) < p_order)) {
          warning(
            "`thresholds_minimums` does not increase as `period` increases, which ",
            "may be in error. Ensure that the thresholds are passed correctly.",
            call. = FALSE
          )
        }
        thresholds_minimums <- thresholds_minimums[p_order]
      }
    }
  }

  # also check the same for first since

  if (!null_fs) {
    fs_order <- order(first_since, decreasing = TRUE)
    first_since <- first_since[fs_order]
    if (is.null(first_since_minimums)) {
      first_since_minimums <- -Inf
    } else {
      ln_fsm <- length(first_since_minimums)
      ln_fs <- length(first_since)
      if (ln_fs != ln_fsm && (ln_fsm != 1)) {
        stop(
          "`first_since_minimum` must have the same length ",
          "as `first_since` if passed to `generate_alerts()`, unless length 1, in ",
          " which case it is recycled.",
          call. = FALSE
        )
      }

      # ensuring order is correct
      if (ln_fsm > 1) {
        first_since_minimums <- first_since_minimums[fs_order]
      }
    }
  }

  # create the rolling sum columns necessary for flagging
  rs_periods <- c(first_since, periods)
  for (period in rs_periods) {
    .data <- dplyr$mutate(
      .data,
      "rs_{{ period }}" := zoo$rollsumr(
        x = .data[[x_col]],
        k = !!period,
        fill = NA
      )
    )
  }

  # if first since is passed as an argument
  # then we loop throw those periods and for each one
  # check when we had a day with value above x
  # and 0 in the previous # number of dates
  if (!null_fs) {
    # create the forward looking 60 day period for checking the minimums
    .data <- dplyr$mutate(
      .data,
      rs_next_60 = zoo$rollsum(
        x = .data[[x_col]],
        k = 60,
        fill = "extend",
        align = "left"
      )
    )

    # here we map across the data and only keep the generated columns each time
    # so we can bind back to the original data frame
    # this is so we don't require specific columns passed in originally, so we
    # bind cols rather than join

    df_fs <- purrr$map2(
      .x = first_since,
      .y = first_since_minimums,
      .f = \(fs_period, fs_minimum) {
        dplyr$mutate(
          .data,
          "flag_first_{{ fs_period }}" := .data[[x_col]] > 0 & dplyr$lag(.data[[paste0("rs_", fs_period)]]) == 0 & rs_next_60 >= fs_minimum
        ) |>
          dplyr$ungroup() |>
          dplyr$select(paste0("flag_first_", fs_period))
      }
    ) |>
      dplyr$bind_cols()

    .data <- dplyr$bind_cols(.data, df_fs)
  }

  # if period is passed as an argument
  # then we map across all of those periods, percent thresholds, static thresholds
  # and minimums to generate flags using flag_fun
  if (!null_p) {
    new_df <- purrr$pmap(
      .l = list(
        periods,
        thresholds_pcts,
        thresholds_static,
        thresholds_minimums
      ),
      .f = \(period, pct, threshold, minimum) {
        dplyr$mutate(
          .data,
          "flag_anomaly_{{ period }}" := flag_fun(
            x = .data[[paste0("rs_", period)]],
            pct = pct,
            threshold = threshold,
            minimum = minimum
          )
        ) |>
          dplyr$ungroup() |>
          dplyr$select(
            starts_with("flag_anomaly_")
          )
      }
    ) |>
      purrr$list_cbind()

    .data <- dplyr$bind_cols(.data, new_df) |>
      dplyr$mutate(
        across(
          .cols = dplyr$starts_with("flag_"),
          .fns = \(x) tidyr$replace_na(x, FALSE)
        )
      )
  }

  # find the flag levels for each time point
  # the higher the level, the more critical the flag
  # this includes the flag first variables (which are the lowest levels of alert)
  # all the way to the longest time period

  # what we do below is select only the numeric flag columns and then for speed
  # convert those to a matrix and find the last column with a 1 (since boolean
  # flags converted to numeric)
  # so we end up with a column from 0 to length(flags) that means we can
  # later easily see when alerts are increasing (this value increases!)

  .data$alert_levels <- .data |>
    dplyr$ungroup() |>
    dplyr$select(
      dplyr$matches("^flag_(first|anomaly)_\\d+$")
    ) |>
    data.matrix() |>
    apply(
      MARGIN = 1,
      FUN = \(x) max(which(x == 1), -Inf)
    ) |>
    pmax( # make the minimum 0
      0
    )

  .data$alert_any <- .data$alert_levels > 0

  # generate an alert if the flag level has increased
  # from a previous time point
  # and group by these various alerts

  .data
}

#' Generate alert metadata
#'
#' Generates alerts from a flagging data frame output by `calculate_flags()`.
#' Works by
#'
#' @param .data Data frame, output of `calculate_flags()`
#' @param date_col `character` name of date vector.
#' @param x `character` name of numeric vector that was used for flagging.
#'
#' @return Data frame with flags and flag metadata
#'
#' @export
generate_alerts <- function(
    .data,
    date_col,
    x_col
) {
  # automatically detect flag columns and the days used to generate
  col_names <- names(.data)
  flag_cols <- col_names[stringr$str_detect(col_names, "^flag_")]
  flag_days <- ifelse(
    stringr$str_detect(flag_cols, "^flag_first"),
    0,
    readr$parse_number(flag_cols)
  )

  # get raw data that can be used to accurately capture
  # flag_dates and displacement for each alert
  df_x <- dplyr$select(
    .data,
    dplyr$group_cols(),
    dplyr$all_of(c(date_col, x_col))
  )

  # use the output of calculate_flags and now generate alert metadata
  # including start and end date for the alerts, start and end date for
  # the data itself, and total displacement, useful for messaging
  # later on
  .data |>
    dplyr$mutate(
      alert = alert_levels - dplyr$lag(alert_levels, default = 0) > 0,
      alert_group = cumsum(alert_levels != dplyr$lag(alert_levels, default = 0))
    ) |>
    dplyr$group_by(
      alert_group,
      .add = TRUE
    ) |>
    dplyr$filter(
      any(alert)
    ) |>
    dplyr$summarize(
      alert_days = flag_days[unique(alert_levels)], # use flag_days as calculated above to ensure correct days used for each column
      alert_name = flag_cols[unique(alert_levels)], # alert_levels will always have a single unique value per group as defined above in alert_group
      alert_start_date = min(.data[[date_col]]),
      alert_end_date = max(.data[[date_col]]),
      x_start_date = alert_start_date - lubridate$days(alert_days),
      .groups = "keep"
    ) |>
    dplyr$full_join(
      y = df_x,
      by = dplyr$group_vars(df_x),
      relationship = "many-to-many"
    ) |>
    dplyr$filter( # filter between start and end dates and only positive displacement days so the final start date starts when displacement starts
      .data[[date_col]] >= x_start_date,
      .data[[date_col]] <= alert_end_date + lubridate$days(60), # use so data_sum has all data when reported (emails can't go out after 60 days)
      .data[[x_col]] > 0
    ) |>
    dplyr$summarize(
      alert_name = unique(alert_name),
      alert_start_date = unique(alert_start_date),
      alert_end_date = unique(alert_end_date),
      data_start_date = min(.data[[date_col]]),
      data_end_date = max(.data[[date_col]]),
      data_sum = sum(.data[[x_col]]),
      .groups = "drop"
    )
}

#' Helper function for generating flags
#'
#' Generates a flag when `x` is above `pct` percent of historic data or the
#' static `threshold`, whichever is lower.
#'
#' @param x `numeric` vector to flag.
#' @param pct `numeric` Percent anomaly (passed to quantile) that will generate
#'     a flag if `x` is above.
#' @param threshold `numeric` threshold that generates flag if `x` above
#'     this static value.
#' @param minimum `numeric` minimum value where no flag is generated unless
#'     `x` is above this value.
flag_fun <- function(x, pct, threshold, minimum) {
  # use lowest of percent threshold or static
  threshold <- min(
    stats$quantile(x, probs = pct, na.rm = TRUE),
    threshold
  )

  x >= max(threshold, minimum)
}
