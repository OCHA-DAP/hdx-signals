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

#' Share of admin 1 units qualifying for each candidate signal
#'
#' Pivots `dry_rp` and `wet_rp` into a `direction` column and, for every
#' country, issuance, trimester and direction, counts the units that satisfy
#' `unit_qualifies()`. Only leads of at least `seas5_min_lead` are kept. Rows
#' are ordered so the first row per `iso3` and `date` is the strongest signal:
#' largest qualifying share, then shortest lead, then dry before wet.
#'
#' @param df_wrangled Wrangled data frame
#'
#' @returns Data frame with `iso3`, `date`, `trimester`, `season_year`, `lead`,
#'     `direction`, `n_units`, `n_qualifying` and `frac_qualifying`
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
      n_units = dplyr$n(),
      n_qualifying = sum(unit_qualifies(rp, pearson_r, in_season_flat)),
      .groups = "drop"
    ) |>
    dplyr$mutate(frac_qualifying = n_qualifying / n_units) |>
    dplyr$arrange(iso3, date, dplyr$desc(frac_qualifying), lead, direction)
}

#' Creates SEA5 anomaly alerts dataset
#'
#' Evaluates the dry and wet signals separately for every country, issuance and
#' forecast trimester with `signal_shares()`. A signal fires when at least 60% of
#' the country's admin 1 units qualify.
#'
#' Since campaigns are keyed on `iso3` and `date`, a country that fires for
#' several trimesters or both directions in one issuance keeps only its
#' strongest signal. The chosen `direction`, `trimester`, `season_year`, `lead`
#' and unit counts are carried alongside the required alert columns.
#'
#' @param df_wrangled Wrangled data frame
#'
#' @returns Alerts dataset
#'
#' @export
alert <- function(df_wrangled) {
  df_wrangled |>
    signal_shares() |>
    dplyr$filter(frac_qualifying >= seas5_frac_units) |>
    dplyr$distinct(iso3, date, .keep_all = TRUE) |>
    dplyr$transmute(
      iso3,
      indicator_name = "anomaly",
      indicator_source = "sea5",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      date,
      alert_level_numeric = 1L,
      value = frac_qualifying,
      direction,
      trimester,
      season_year,
      lead,
      n_units,
      n_qualifying,
      title = paste0(
        scales$label_percent(accuracy = 1)(value),
        " of admin 1 areas forecast an unusually ", direction, " ",
        trimester, " ", season_year, " season (1-in-", seas5_rp_years,
        "-year event or rarer)"
      ),
      extreme_case = FALSE
    )
}
