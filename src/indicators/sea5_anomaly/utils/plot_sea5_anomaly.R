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

# Bars are positioned by `series` (two per trimester) and coloured by
# `fill_class`, which splits the forecast by anomaly direction. Alerting
# forecast bars are outlined in red on top of their direction colour, and
# off-season trimesters are striped.
series_levels <- c("Historical average", "Forecast")
direction_labels <- c(dry = "Dry forecast", wet = "Wet forecast")
season_patterns <- c("TRUE" = "none", "FALSE" = "stripe")
alert_outline <- c("TRUE" = hdx_signals_palette$danger_red)

# The outline and stripe encodings are folded into the fill legend as two extra
# keys ("Alerting", "Off season") so the plot carries a single legend row.
# `legend_keys` sets the outline and pattern of each key in `fill_colors` order.
fill_colors <- c(
  "Historical" = hdx_signals_palette$map_boundary,
  "Dry forecast" = palette_sea5_anomaly$direction_colors[["dry"]],
  "Wet forecast" = palette_sea5_anomaly$direction_colors[["wet"]],
  "Alerting" = "white",
  "Off season" = hdx_signals_palette$map_boundary
)
legend_keys <- list(
  colour = c(NA, NA, NA, alert_outline[["TRUE"]], NA),
  pattern = c("none", "none", "none", "none", "stripe"),
  linewidth = 0.8
)
dodge_width <- 0.9
bar_width <- 0.42

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
#' Averages `forecast_mm` and `hist_mean_mm` across the country's admin 1 units
#' and takes the median dry and wet return periods, for the latest issuance up
#' to the alert date and fully forecast trimesters only. The return period shown
#' for a trimester is in the direction of the country signal when the trimester
#' alerts (60% or more of units qualifying, see `alert_sea5_anomaly$alert()`),
#' otherwise in the direction of the mean forecast relative to the historical
#' average. A trimester is off season when fewer than half of the units are in
#' their rainy season.
#'
#' @param df_wrangled Wrangled data frame for a single country
#'
#' @returns Data frame with `trimester` (ordered factor for the x axis),
#'     `forecast_mm`, `hist_mean_mm`, `in_season`, `direction`, `rp` and
#'     `alerting`
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
    dplyr$summarise(
      forecast_mm = mean(forecast_mm, na.rm = TRUE),
      hist_mean_mm = mean(hist_mean_mm, na.rm = TRUE),
      dry_rp = stats$median(dry_rp, na.rm = TRUE),
      wet_rp = stats$median(wet_rp, na.rm = TRUE),
      in_season = mean(in_season_flat, na.rm = TRUE) >= 0.5,
      .groups = "drop"
    ) |>
    dplyr$left_join(df_alerting, by = "trimester") |>
    dplyr$mutate(
      alerting = !is.na(alert_direction),
      direction = dplyr$coalesce(
        alert_direction,
        dplyr$if_else(forecast_mm < hist_mean_mm, "dry", "wet")
      ),
      rp = dplyr$if_else(direction == "dry", dry_rp, wet_rp),
      trimester = forcats$fct_reorder(paste(trimester, season_year, sep = "\n"), lead)
    )
}

#' Plot SEA5 anomaly data for a single country
#'
#' Grouped bar chart with, for every trimester of the issuance, the historical
#' average rainfall next to the forecast rainfall, both as country means of the
#' admin 1 trimester totals in mm. Forecast bars are filled by anomaly
#' direction, labelled with the median return period of the anomaly and
#' outlined in red when the trimester alerts. Off-season trimesters are
#' striped. See `summarise_trimesters()` for the aggregation.
#'
#' @param df_wrangled Wrangled data frame for plotting (single alerted country).
#' @param df_raw Raw data frame, not used.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns Bar chart of forecast against historical rainfall by trimester
sea5_anomaly_plot <- function(df_wrangled, df_raw, title, date) {
  df_trimesters <- summarise_trimesters(df_wrangled)

  df_bars <- df_trimesters |>
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
      fill_class = factor(
        dplyr$if_else(series == "Forecast", direction_labels[direction], "Historical"),
        levels = names(fill_colors)
      ),
      # only forecast bars can alert; NA draws no outline
      outline = dplyr$if_else(series == "Forecast" & alerting, "TRUE", NA_character_)
    )

  df_labels <- df_bars |>
    dplyr$filter(series == "Forecast") |>
    dplyr$mutate(
      label = paste(scales$number(rp, accuracy = 0.1, drop0trailing = TRUE), "RP")
    )

  gg$ggplot(
    data = df_bars,
    mapping = gg$aes(x = trimester, y = mm, group = series)
  ) +
    ggpattern$geom_col_pattern(
      mapping = gg$aes(fill = fill_class, color = outline, pattern = as.character(in_season)),
      position = gg$position_dodge(width = dodge_width),
      width = bar_width,
      linewidth = 0.8,
      pattern_fill = "white",
      pattern_colour = "white",
      pattern_angle = 45,
      pattern_density = 0.3,
      pattern_spacing = 0.03,
      pattern_key_scale_factor = 0.5,
      # draw a swatch for every class, including those absent from this
      # country, so the legend reads the same across campaigns
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
    gg$scale_fill_manual(
      values = fill_colors,
      drop = FALSE
    ) +
    gg$scale_color_manual(
      values = alert_outline,
      na.value = NA,
      guide = "none"
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
      fill = "",
      title = title,
      caption = caption$caption(
        indicator_id = "sea5_anomaly",
        iso3 = unique(df_wrangled$iso3),
        extra_caption = paste(
          "Country means of admin 1 trimester totals.",
          "Labels: median return period (years) of the forecast anomaly.",
          sep = "\n"
        )
      )
    ) +
    gg$guides(
      fill = gg$guide_legend(override.aes = legend_keys)
    ) +
    theme_signals$theme_signals() +
    gg$theme(
      axis.line.x = gg$element_blank(),
      panel.grid.major.x = gg$element_blank(),
      legend.position = "top",
      legend.justification = "left"
    )
}
