library(tidyverse)

#' Helper function for generating flags
#'
#' Generates a flag when `x` is above `pct` percent of historic data or the
#' static `threshold`, whichever is lower.
#'
#' @param x Vector to flag
#' @param pct Percent anomaly (passed to quantile) that will generate a flag
#'     if `x` is above.
#' @param threshold Flag if `x` above this static value
#' @param minimum
flag_fun <- function(x, pct, threshold, minimum) {
  # use lowest of percent threshold or static
  threshold <- min(
    stats::quantile(x, probs = pct, na.rm = TRUE),
    threshold
  )

  x >= max(threshold, minimum)
}

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
#' @param date Date column, must be daily data.
#' @param x Numeric column to flag
#' @param first_since Numeric vector indicating when to flag the first positive
#'     values. For instance, `180` would indicate flagging whenever there is a
#'     positive value after 180 days.
#' @param periods Numeric vector of times that flags are calculated, used
#'     for all of the `thresholds_...` arguments. For instance, `c(7, 30, 90)`
#'     would specify that flags are calculated for rolling sums of 7 days,
#'     30 days, and 90 days.
#' @param thresholds_pcts Numeric vector of percentile to use for flagging,
#'     so that any time the value for `x` is over that percent quantile based
#'     on the historic data, a flag is
#'     generated. If a single value, is recycled, otherwise must have the
#'     same length as `periods`.
#' @param thresholds_static Numeric vector of static thresholds to use for
#'     flagging, so that any time the value of `x` is above that value, a flag
#'     is generated. If a single value, is recycled, otherwise must have the
#'     same length as `periods`.
#' @param thresholds_minimums Numeric vector of minimum values such that if `x`
#'     is above it, no flags are generated. If a single value, is recyled,
#'     otherwise must have the same length as `periods`.
#'
#' @return Data frame with rolling sums and flags generated.
calculate_flags <- function(
    .data,
    date,
    x,
    first_since = NULL,
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
        "passed to `calculate_alerts()` if `periods` is ",
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
        "as `periods` if passed to `calculate_alerts()`, unless length 1, in ",
        " which case they are recycled.",
        call. = FALSE
      )
    }

    # reorderig for later pmap
    if (ln_tp > 1) {
      thresholds_pcts <- thresholds_pcts[p_order]
    }

    if (ln_ts > 1) {
      thresholds_static <- thresholds_static[p_order]
    }

    if (is.null(thresholds_minimums)) {
      thresholds_minimums <- 0
    } else {
      ln_m <- length(thresholds_minimums)
      if (ln_p != ln_m && (ln_m != 1)) {
        stop(
          "`thresholds_minimum` must have the same length ",
          "as `periods` if passed to `calculate_alerts()`, unless length 1, in ",
          " which case it is recycled.",
          call. = FALSE
        )
      } else if (ln_m > 1) {
        thresholds_minimums <- thresholds_minimums[p_order]
      }
    }
  }

  # create the rolling sum columns necessary for flagging
  rs_periods <- c(first_since, periods)
  for (period in rs_periods) {
    .data <- mutate(
      .data,
      "rs_{{ period }}" := zoo::rollsumr(
        x = {{ x }},
        k = !!period,
        fill = NA
      )
    )
  }

  # if first since is passed
  if (!null_fs) {
    first_since <- sort(first_since, decreasing = FALSE)
    for (period in first_since) {
      .data <- mutate(
        .data,
        "flag_first_{{ period }}" := {{ x }} > 0 & lag(.data[[paste0("rs_", period)]]) == 0
      )
    }
  }

  # if period is passed
  if (!null_p) {
    new_df <- pmap(
      .l = list(
        periods,
        thresholds_pcts,
        thresholds_static,
        thresholds_minimums
      ),
      .f = \(period, pct, threshold, minimum) {
        mutate(
          .data,
          "flag_anomaly_{{ period }}" := flag_fun(
            x = .data[[paste0("rs_", period)]],
            pct = pct,
            threshold = threshold,
            minimum = minimum
          )
        ) %>%
          ungroup() %>%
          select(
            starts_with("flag_anomaly_")
          )
      }
    ) %>%
      list_cbind()

    .data <- bind_cols(.data, new_df) %>%
      mutate(
        across(
          .cols = starts_with("flag_"),
          .fns = \(x) replace_na(x, FALSE)
        )
      )
  }

  # find the flag levels for each time point
  # the higher the level, the more critical the flag

  .data$alert_levels <- .data %>%
    ungroup() %>%
    select(
      starts_with("flag_")
    ) %>%
    data.matrix() %>%
    apply(
      MARGIN = 1,
      FUN = \(x) max(which(x == 1), -Inf)
    ) %>%
    pmax(
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
#' @param .data Data frame, output of `calculate_flags()`
#' @inheritParams calculate_flags
#'
#' @return Data frame with flags and flag metadata
wrangle_alerts <- function(
    .data,
    date,
    x
) {
  # automatically detect flag columns and the days used to generate
  col_names <- names(.data)
  flag_cols <- col_names[str_detect(col_names, "^flag_")]
  flag_days <- ifelse(
    str_detect(flag_cols, "^flag_first"),
    0,
    parse_number(flag_cols)
  )

  # get displacement data that can be used to accurately capture
  # flag dates and total displacement for each alert
  df_displacement <- select(
    .data,
    group_cols(),
    {{ date }},
    {{ x }}
  )

  # use the output of calculate_flags and now generate alert metadata
  # including start and end date for the alerts, start and end date for
  # the data itself, and total displacement, useful for messaging
  # later on
  .data %>%
    mutate(
      alert = alert_levels - lag(alert_levels, default = 0) > 0,
      alert_group = cumsum(alert_levels != lag(alert_levels, default = 0))
    ) %>%
    group_by(
      alert_group,
      .add = TRUE
    ) %>%
    filter(
      any(alert)
    ) %>%
    summarize(
      alert_days = flag_days[unique(alert_levels)],
      alert_name = flag_cols[unique(alert_levels)],
      alert_start_date = min({{ date }}),
      alert_end_date = max({{ date }}),
      displacement_start_date = alert_start_date - days(alert_days),
      .groups = "keep"
    ) %>%
    full_join(
      y = df_displacement,
      by = group_vars(df_displacement),
      relationship = "many-to-many"
    ) %>%
    filter(
      {{ date }} >= displacement_start_date,
      {{ date }} <= alert_end_date,
      {{ x }} > 0
    ) %>%
    summarize(
      alert_name = unique(alert_name),
      alert_start_date = unique(alert_start_date),
      alert_end_date = unique(alert_end_date),
      data_start_date = min({{ date }}),
      data_end_date = max({{ date }}),
      data_sum = sum({{ x }}),
      .groups = "drop"
    )
}
