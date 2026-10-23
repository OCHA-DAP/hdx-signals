box::use(
  dplyr,
  scales,
  tidyr
)

# Signal parameters from the ds-seas5-skill hand-over specification
# (https://ocha-dap.github.io/ds-seas5-skill/hdx-signal/). Leads below 0 are
# in-season trimesters that blend observations into the forecast; they are
# excluded so alerts stay anticipatory and avoid the inflated in-season skill.
#' @export
seas5_frac_units <- 0.6

#' @export
seas5_rp_years <- 5

seas5_r_min <- 0.3

#' @export
seas5_min_lead <- 0L

#' @export
seas5_min_units <- 3L

#' Format a return period for labels and text
#'
#' @param rp Return period in years
#'
#' @returns Character vector, one decimal at most, e.g. `"7.7"` or `"46"`
#'
#' @export
format_rp <- function(rp) {
  scales$number(rp, accuracy = 0.1, drop0trailing = TRUE)
}

#' Does an admin 1 unit qualify for the signal
#'
#' A unit qualifies when its return period in the signal direction is at least
#' `seas5_rp_years`, its hindcast skill is at least `seas5_r_min` and the
#' trimester is in its rainy season. Missing values never qualify.
#'
#' @param rp Return period in the signal direction (`dry_rp` or `wet_rp`)
#' @param pearson_r Detrended hindcast skill
#' @param in_season_flat Whether the trimester is in the unit's rainy season
#'
#' @returns Logical vector
#'
#' @export
unit_qualifies <- function(rp, pearson_r, in_season_flat) {
  (rp >= seas5_rp_years & pearson_r >= seas5_r_min & in_season_flat) %in% TRUE
}

#' Share of in-season admin 1 units qualifying for each candidate signal
#'
#' Pivots `dry_rp` and `wet_rp` into a `direction` column and, for every
#' country, issuance, trimester and direction, counts the units that satisfy
#' `unit_qualifies()` as a share of the units in their rainy season for that
#' trimester, so a country only partly in season can still signal. Trimesters
#' with no unit in season are dropped. `few_units` flags signals resting on
#' fewer than `seas5_min_units` in-season units. Only leads of at least
#' `seas5_min_lead` are kept. Rows are ordered so the first row per `iso3` and
#' `date` is the strongest signal: largest qualifying share, then shortest
#' lead, then dry before wet.
#'
#' @param df_wrangled Wrangled data frame
#'
#' @returns Data frame with `iso3`, `date`, `trimester`, `season_year`, `lead`,
#'     `direction`, `n_units` (in season), `n_qualifying`, `frac_qualifying`
#'     and `few_units`
#'
#' @export
signal_shares <- function(df_wrangled) {
  df_wrangled |>
    dplyr$filter(lead >= seas5_min_lead) |>
    tidyr$pivot_longer(
      cols = c(dry_rp, wet_rp),
      names_to = "direction",
      names_pattern = "(dry|wet)_rp",
      values_to = "rp"
    ) |>
    dplyr$group_by(iso3, date, trimester, season_year, lead, direction) |>
    dplyr$summarise(
      n_units = sum(in_season_flat %in% TRUE),
      n_qualifying = sum(unit_qualifies(rp, pearson_r, in_season_flat)),
      .groups = "drop"
    ) |>
    dplyr$filter(n_units > 0) |>
    dplyr$mutate(
      frac_qualifying = n_qualifying / n_units,
      few_units = n_units < seas5_min_units
    ) |>
    dplyr$arrange(iso3, date, dplyr$desc(frac_qualifying), lead, direction)
}

#' Firing SEAS5 anomaly signals, one per country, issuance and direction
#'
#' Evaluates the dry and wet signals separately for every country, issuance and
#' forecast trimester with `signal_shares()`. A signal fires when at least 60% of
#' the country's in-season admin 1 units qualify. When several trimesters fire in
#' the same direction, only the first is kept, meaning the shortest lead, then
#' the largest qualifying share.
#'
#' @param df_wrangled Wrangled data frame
#'
#' @returns Data frame in the format of `signal_shares()`, at most one row per
#'     `iso3`, `date` and `direction`
#'
#' @export
firing_signals <- function(df_wrangled) {
  df_wrangled |>
    signal_shares() |>
    dplyr$filter(frac_qualifying >= seas5_frac_units) |>
    dplyr$arrange(iso3, date, direction, lead, dplyr$desc(frac_qualifying)) |>
    dplyr$distinct(iso3, date, direction, .keep_all = TRUE)
}

#' Creates SEAS5 anomaly alerts dataset
#'
#' Dry and wet signals are separate indicators, `seas5_anomaly_dry` and
#' `seas5_anomaly_wet`, so a country can alert once per issuance for each. Alerts
#' are the `firing_signals()` in the requested direction. Signals resting on
#' fewer than `seas5_min_units` in-season units still fire but are flagged with
#' `few_units`, which the title and the summary caveat. The chosen `direction`,
#' `trimester`, `season_year`, `lead`, unit counts and `few_units` are carried
#' alongside the required alert columns.
#'
#' @param df_wrangled Wrangled data frame
#' @param signal_direction Direction of the signals to alert on, `"dry"` or
#'     `"wet"`
#'
#' @returns Alerts dataset
#'
#' @export
alert <- function(df_wrangled, signal_direction = c("dry", "wet")) {
  signal_direction <- match.arg(signal_direction)
  df_wrangled |>
    firing_signals() |>
    dplyr$filter(direction == signal_direction) |>
    dplyr$transmute(
      iso3,
      indicator_name = "anomaly",
      indicator_source = "seas5",
      indicator_id = paste("seas5_anomaly", signal_direction, sep = "_"),
      date,
      alert_level_numeric = 1L,
      value = frac_qualifying,
      direction,
      trimester,
      season_year,
      lead,
      n_units,
      n_qualifying,
      few_units,
      title = paste0(
        scales$label_percent(accuracy = 1)(value),
        " of in-season admin 1 areas forecast an unusually ", direction, " ",
        trimester, " ", season_year, " season (1-in-", seas5_rp_years,
        "-year event or rarer)",
        dplyr$if_else(few_units, paste0(" - only ", n_units, " in season"), "")
      ),
      extreme_case = FALSE
    )
}
