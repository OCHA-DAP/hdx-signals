box::use(
  dplyr,
  glue,
  purrr,
  scales
)

box::use(
  src/indicators/sea5_anomaly/utils/alert_sea5_anomaly,
  src/indicators/sea5_anomaly/utils/plot_sea5_anomaly,
  src/signals/track_summary_input,
  src/utils/get_prompts,
  src/utils/python_setup
)

sea5_anomaly <- "sea5_anomaly"

# admin 1 areas named in the summariser input, strongest anomaly first
max_units <- 5L

#' Add summary to SEA5 anomaly alerts
#'
#' Generates the long and short AI summaries for every alert. Unlike the other
#' indicators there is no narrative source text: the model is given only a
#' structured description of the signal built by `alert_info()` from the alerts
#' and wrangled data of the alerting country. The input and outputs are logged
#' with `track_summary_input`.
#'
#' @param df_alerts Data frame of alerts, with `location` and the signal columns
#'     carried by `alert_sea5_anomaly$alert()`
#' @param df_wrangled Wrangled data frame
#' @param df_raw Raw data frame, not used
#'
#' @returns Data frame with `summary_long`, `summary_short` and `summary_source`
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts(sea5_anomaly)

  df_summary <- df_alerts |>
    dplyr$mutate(
      info = purrr$map_chr(
        split(df_alerts, seq_len(nrow(df_alerts))),
        alert_info,
        df_wrangled = df_wrangled
      ),
      summary_long = purrr$pmap_chr(
        .l = list(
          system_prompt = prompts$system,
          user_prompt = prompts$long,
          info = info
        ),
        .f = python_setup$get_summary_r
      ),
      summary_short = purrr$pmap_chr(
        .l = list(
          system_prompt = prompts$system,
          user_prompt = prompts$short,
          info = summary_long,
          location = location
        ),
        .f = python_setup$get_summary_r
      ),
      summary_source = "ECMWF SEAS5 seasonal forecasts",
      # deterministic caveat so it never depends on the model keeping it
      summary_long = dplyr$if_else(
        few_units,
        paste(summary_long, few_units_caveat(n_units)),
        summary_long
      )
    )

  df_summary |>
    dplyr$transmute(
      location_iso3 = iso3,
      date_generated = date,
      indicator_id = sea5_anomaly,
      info,
      manual_info = NA_character_,
      use_manual_info = FALSE,
      summary_long,
      summary_short,
      summary_source
    ) |>
    track_summary_input$append_tracking_data()

  dplyr$select(df_summary, summary_long, summary_short, summary_source)
}

#' Caveat for signals resting on few in-season areas
#'
#' @param n_units Number of admin 1 areas in season
#'
#' @returns Character vector
few_units_caveat <- function(n_units) {
  glue$glue(
    "Note: only {n_units} of the country's admin 1 areas ",
    "{ifelse(n_units == 1, 'is', 'are')} in season for this trimester, so the ",
    "signal rests on very few areas."
  )
}

#' Describe an alert for the summariser
#'
#' Builds the plain-text input for the AI summary of one alert: the signal
#' (share of admin 1 areas, direction, trimester), the outlook for every
#' forecast trimester from `plot_sea5_anomaly$summarise_trimesters()` and the
#' `max_units` alerting admin 1 areas with the largest return period.
#'
#' @param alert Single-row alerts data frame
#' @param df_wrangled Wrangled data frame
#'
#' @returns Character string
alert_info <- function(alert, df_wrangled) {
  df_country <- dplyr$filter(df_wrangled, iso3 == alert$iso3, date == alert$date)

  trimester_lines <- df_country |>
    plot_sea5_anomaly$summarise_trimesters() |>
    dplyr$mutate(
      status = dplyr$case_when(
        alerting ~ "in season, alerting",
        in_season ~ "in season, not alerting",
        .default = "off season, not assessed"
      ),
      line = glue$glue(
        "- {trimester} {season_year} ({status}): forecast {round(forecast_mm)} mm ",
        "vs {round(hist_mean_mm)} mm historical average, {direction}, ",
        "return period {alert_sea5_anomaly$format_rp(rp)} years."
      )
    ) |>
    dplyr$pull(line)

  unit_line <- df_country |>
    dplyr$filter(trimester == alert$trimester) |>
    dplyr$mutate(rp = if (alert$direction == "dry") dry_rp else wet_rp) |>
    dplyr$filter(alert_sea5_anomaly$unit_qualifies(rp, pearson_r, in_season_flat)) |>
    dplyr$slice_max(rp, n = max_units, with_ties = FALSE) |>
    dplyr$mutate(unit = paste0(name, " (", alert_sea5_anomaly$format_rp(rp), " years)")) |>
    dplyr$pull(unit) |>
    paste(collapse = ", ")

  paste(
    glue$glue(
      "Seasonal rainfall forecast for {alert$location}, ECMWF SEAS5 issued ",
      "{format(alert$date, '%B %Y')}."
    ),
    glue$glue(
      "Signal: {alert$n_qualifying} of the {alert$n_units} admin 1 areas in ",
      "season ({share}) forecast an unusually {alert$direction} ",
      "{alert$trimester} {alert$season_year} rainy season, a 1-in-{rp_years}-year ",
      "event or rarer with at least moderate forecast skill.",
      share = scales$label_percent(accuracy = 1)(alert$value),
      rp_years = alert_sea5_anomaly$seas5_rp_years
    ),
    paste(
      "Outlook by trimester (mean over the admin 1 areas in season, or all areas",
      "when none is; forecast vs historical average rainfall; median return",
      "period of the anomaly):"
    ),
    paste(trimester_lines, collapse = "\n"),
    glue$glue(
      "Admin 1 areas with the strongest anomaly for {alert$trimester} ",
      "{alert$season_year}, with return period: {units}.",
      units = unit_line
    ),
    sep = "\n"
  )
}
