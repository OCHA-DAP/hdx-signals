box::use(
  dplyr,
  forcats,
  gg = ggplot2,
  ggpattern,
  scales,
  stats,
  tidyr
)

box::use(
  src/images/create_images,
  src/images/plots/caption,
  src/images/plots/hdx_signals_palette,
  src/images/plots/theme_signals,
  src/indicators/sea5_anomaly/utils/alert_sea5_anomaly,
  src/indicators/sea5_anomaly/utils/palette_sea5_anomaly
)

# Bars are positioned by `series` (two per trimester). Forecast bars are filled
# on a continuous gradient of the signed return period (negative for dry,
# positive for wet): grey at zero, the map's light direction shade at the
# signal threshold and its darkest shade at `rp_limit`, so a weak anomaly is a
# grey-tinted orange or blue. Historical bars are a constant grey (`NA` on the
# gradient).
series_levels <- c("Historical average", "Forecast")
rp_limit <- 20 # the gradient saturates here, matching the map's darkest class
threshold <- alert_sea5_anomaly$seas5_rp_years
gradient_stops <- c(-rp_limit, -10, -threshold, 0, threshold, 10, rp_limit)
gradient_colors <- c(
  rev(palette_sea5_anomaly$direction_palettes$dry),
  hdx_signals_palette$hairline,
  palette_sea5_anomaly$direction_palettes$wet
)
gradient_breaks <- c(-rp_limit, -10, -threshold, threshold, 10, rp_limit)
gradient_labels <- c(paste0("Dry ", rp_limit, "+"), "10", threshold, threshold, "10", paste0("Wet ", rp_limit, "+"))
historical_fill <- hdx_signals_palette$text_muted

# The outline, stripe and historical encodings share one small legend driven by
# the `colour` aesthetic; `legend_keys` sets each key's look in `key_levels` order
key_levels <- c("Historical", "Alerting", "Off season")
key_outline <- c("Historical" = NA, "Alerting" = hdx_signals_palette$danger_red, "Off season" = NA)
legend_keys <- list(
  fill = c(historical_fill, "white", hdx_signals_palette$map_boundary),
  colour = key_outline,
  pattern = c("none", "none", "stripe")
)
season_patterns <- c("TRUE" = "none", "FALSE" = "stripe")
dodge_width <- 0.85
bar_width <- 0.8 # halved by the dodge, so each bar is 0.4 wide with a 0.025 gap

#' Plot SEA5 anomaly
#'
#' Plots the forecast and historical average rainfall of the country for every
#' trimester of the issuance.
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw Raw data frame
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  df_plot <- df_alerts |>
    dplyr$mutate(
      title = paste("Rainfall forecast,", format(date, "%B %Y"), "issuance")
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = sea5_anomaly_plot,
    image_use = "plot",
    height = 4,
    width = 6
  )
}

#' Summarise the issuance to one row per trimester
#'
#' Averages `forecast_mm` and `hist_mean_mm` and takes the median dry and wet
#' return periods across the admin 1 units in their rainy season for each
#' trimester, the same units the signal assesses, for the latest issuance up to
#' the alert date and fully forecast trimesters only. A trimester is off season
#' when no unit is in its rainy season; it is then summarised over all units so
#' it can still be drawn, and the signal does not assess it. The return period
#' shown for a trimester is in the direction of the country signal when the
#' trimester alerts (60% or more of in-season units qualifying, see
#' `alert_sea5_anomaly$alert()`), otherwise in the direction of the mean
#' forecast relative to the historical average.
#'
#' @param df_wrangled Wrangled data frame for a single country
#'
#' @returns Data frame ordered by `lead` with `trimester`, `season_year`,
#'     `forecast_mm`, `hist_mean_mm`, `in_season`, `direction`, `rp` and
#'     `alerting`
#'
#' @export
summarise_trimesters <- function(df_wrangled) {
  df_issuance <- dplyr$filter(
    df_wrangled,
    date == max(date),
    lead >= alert_sea5_anomaly$seas5_min_lead
  )

  df_alerting <- df_issuance |>
    alert_sea5_anomaly$signal_shares() |>
    dplyr$filter(frac_qualifying >= alert_sea5_anomaly$seas5_frac_units) |>
    dplyr$distinct(trimester, alert_direction = direction)

  df_issuance |>
    dplyr$group_by(lead, trimester, season_year) |>
    dplyr$mutate(in_season = any(in_season_flat %in% TRUE)) |>
    # in-season units only, unless the whole country is off season
    dplyr$filter(!in_season | in_season_flat %in% TRUE) |>
    dplyr$summarise(
      forecast_mm = mean(forecast_mm, na.rm = TRUE),
      hist_mean_mm = mean(hist_mean_mm, na.rm = TRUE),
      dry_rp = stats$median(dry_rp, na.rm = TRUE),
      wet_rp = stats$median(wet_rp, na.rm = TRUE),
      in_season = dplyr$first(in_season),
      .groups = "drop"
    ) |>
    dplyr$left_join(df_alerting, by = "trimester") |>
    dplyr$mutate(
      alerting = !is.na(alert_direction),
      direction = dplyr$coalesce(
        alert_direction,
        dplyr$if_else(forecast_mm < hist_mean_mm, "dry", "wet")
      ),
      rp = dplyr$if_else(direction == "dry", dry_rp, wet_rp)
    ) |>
    dplyr$arrange(lead)
}

#' Plot SEA5 anomaly data for a single country
#'
#' Grouped bar chart with, for every trimester of the issuance, the historical
#' average rainfall next to the forecast rainfall, both as means of the admin 1
#' trimester totals in mm over the areas in season. Forecast bars are coloured
#' on a continuous dry-to-wet gradient of the return period of their anomaly,
#' grey below the signal threshold, labelled with the median return period and
#' outlined in red when the trimester alerts. Off-season trimesters are striped.
#' See `summarise_trimesters()` for the aggregation.
#'
#' @param df_wrangled Wrangled data frame for plotting (single alerted country).
#' @param df_raw Raw data frame, not used.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns Bar chart of forecast against historical rainfall by trimester
sea5_anomaly_plot <- function(df_wrangled, df_raw, title, date) {
  df_bars <- summarise_trimesters(df_wrangled) |>
    dplyr$mutate(
      trimester = forcats$fct_reorder(paste(trimester, season_year, sep = "\n"), lead)
    ) |>
    tidyr$pivot_longer(
      cols = c(hist_mean_mm, forecast_mm),
      names_to = "series",
      values_to = "mm"
    ) |>
    dplyr$mutate(
      series = factor(
        dplyr$if_else(series == "forecast_mm", "Forecast", "Historical average"),
        levels = series_levels
      ),
      # historical bars sit outside the gradient and take `na.value`
      rp_signed = dplyr$case_when(
        series == "Historical average" ~ NA_real_,
        direction == "dry" ~ -rp,
        .default = rp
      ),
      key = factor(
        dplyr$case_when(
          series == "Forecast" & alerting ~ "Alerting",
          series == "Historical average" ~ "Historical",
          !in_season ~ "Off season",
          .default = NA_character_
        ),
        levels = key_levels
      )
    )

  df_labels <- df_bars |>
    dplyr$filter(series == "Forecast") |>
    dplyr$mutate(label = paste(alert_sea5_anomaly$format_rp(rp), "y RP"))

  gg$ggplot(
    data = df_bars,
    mapping = gg$aes(x = trimester, y = mm, group = series)
  ) +
    ggpattern$geom_col_pattern(
      mapping = gg$aes(fill = rp_signed, color = key, pattern = as.character(in_season)),
      position = gg$position_dodge(width = dodge_width),
      width = bar_width,
      linewidth = 0.7,
      pattern_fill = "white",
      pattern_colour = "white",
      pattern_angle = 45,
      pattern_density = 0.3,
      pattern_spacing = 0.03,
      pattern_key_scale_factor = 0.5,
      # draw every legend key, including those absent from this country, so
      # the legend reads the same across campaigns
      show.legend = TRUE
    ) +
    gg$geom_text(
      data = df_labels,
      mapping = gg$aes(label = label),
      # centre the label over the forecast bar, the right-hand bar of the pair
      position = gg$position_nudge(x = dodge_width / 4),
      vjust = -0.5,
      size = 2.8,
      family = "Roboto",
      fontface = dplyr$if_else(df_labels$alerting, "bold", "plain"),
      color = dplyr$if_else(
        df_labels$alerting,
        hdx_signals_palette$text_headline,
        hdx_signals_palette$text_muted
      )
    ) +
    gg$scale_fill_gradientn(
      colours = gradient_colors,
      values = scales$rescale(gradient_stops, from = c(-rp_limit, rp_limit)),
      limits = c(-rp_limit, rp_limit),
      oob = scales$squish,
      breaks = gradient_breaks,
      labels = gradient_labels,
      na.value = historical_fill
    ) +
    gg$scale_color_manual(
      values = key_outline,
      limits = key_levels,
      na.value = NA
    ) +
    ggpattern$scale_pattern_manual(
      values = season_patterns,
      guide = "none"
    ) +
    gg$scale_y_continuous(
      labels = scales$label_number(),
      expand = gg$expansion(mult = c(0, 0.15))
    ) +
    gg$labs(
      x = "",
      y = "Rainfall (mm, trimester total)",
      fill = "Forecast return period (years)",
      color = "",
      title = title,
      caption = caption$caption(
        indicator_id = "sea5_anomaly",
        iso3 = unique(df_wrangled$iso3),
        extra_caption = "Means over the admin 1 areas in season each trimester."
      )
    ) +
    gg$guides(
      fill = gg$guide_colourbar(
        order = 1,
        theme = gg$theme(
          legend.key.width = gg$unit(1.8, "in"),
          legend.key.height = gg$unit(0.12, "in"),
          legend.title.position = "top"
        ),
        frame.colour = hdx_signals_palette$hairline,
        ticks.colour = hdx_signals_palette$map_label
      ),
      color = gg$guide_legend(order = 2, override.aes = legend_keys)
    ) +
    theme_signals$theme_signals() +
    gg$theme(
      axis.line.x = gg$element_blank(),
      panel.grid.major.x = gg$element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.box = "horizontal",
      legend.box.just = "bottom"
    )
}
