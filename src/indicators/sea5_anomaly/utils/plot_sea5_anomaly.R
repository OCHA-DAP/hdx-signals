box::use(
  dplyr,
  forcats,
  gg = ggplot2,
  stats,
  tidyr
)

box::use(
  src/images/create_images,
  src/images/plots/caption,
  src/images/plots/hdx_signals_palette,
  src/images/plots/theme_signals,
  src/indicators/sea5_anomaly/utils/alert_sea5_anomaly
)

direction_colors <- c(dry = "#7F5619", wet = hdx_signals_palette$primary_blue)
direction_labels <- c(dry = "Dry", wet = "Wet")
season_shapes <- c("In season" = 16, "Off season" = 1)

#' Plot SEA5 anomaly
#'
#' Plots the dry and wet return periods of the country's forecast for every
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
      title = paste("Forecast return periods,", format(date, "%B %Y"), "issuance")
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

#' Plot SEA5 anomaly data for a single country
#'
#' One dot per trimester and direction, showing the median return period across
#' the country's admin 1 units for the latest issuance up to the alert date.
#' Only fully forecast trimesters are shown, so the x axis starts with the
#' trimester beginning in the issuance month and runs to the end of the SEAS5
#' horizon. In-season trimesters (negative leads) blend observations into the
#' forecast and are left out, matching the alert logic. A dot is drawn filled
#' when most units are in their rainy season and hollow otherwise. The dashed
#' line marks the signal's return period threshold.
#'
#' @param df_wrangled Wrangled data frame for plotting (single alerted country).
#' @param df_raw Raw data frame, not used.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns Dot plot of return periods by trimester
sea5_anomaly_plot <- function(df_wrangled, df_raw, title, date) {
  df_plot <- df_wrangled |>
    dplyr$filter(
      date == max(date),
      lead >= alert_sea5_anomaly$seas5_min_lead
    ) |>
    tidyr$pivot_longer(
      cols = c(dry_rp, wet_rp),
      names_to = "direction",
      names_pattern = "(dry|wet)_rp",
      values_to = "rp"
    ) |>
    dplyr$group_by(lead, trimester, season_year, direction) |>
    dplyr$summarise(
      rp = stats$median(rp, na.rm = TRUE),
      season = dplyr$if_else(
        mean(in_season_flat, na.rm = TRUE) >= 0.5, "In season", "Off season"
      ),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      trimester = forcats$fct_reorder(paste(trimester, season_year, sep = "\n"), lead)
    )

  gg$ggplot(
    data = df_plot,
    mapping = gg$aes(x = trimester, y = rp, color = direction, shape = season)
  ) +
    gg$geom_hline(
      yintercept = alert_sea5_anomaly$seas5_rp_years,
      linetype = "dashed",
      color = hdx_signals_palette$text_muted
    ) +
    gg$geom_point(
      size = 3,
      stroke = 1,
      position = gg$position_dodge(width = 0.4)
    ) +
    gg$scale_color_manual(
      values = direction_colors,
      labels = direction_labels
    ) +
    gg$scale_shape_manual(
      values = season_shapes
    ) +
    gg$scale_y_continuous(
      labels = \(x) paste0("1 in ", x),
      limits = c(1, NA),
      expand = gg$expansion(mult = c(0, 0.1))
    ) +
    gg$labs(
      x = "",
      y = "Return period (years)",
      color = "Anomaly",
      shape = "Rainy season",
      title = title,
      caption = caption$caption(
        indicator_id = "sea5_anomaly",
        iso3 = unique(df_wrangled$iso3)
      )
    ) +
    theme_signals$theme_signals() +
    gg$guides(
      color = gg$guide_legend(override.aes = list(shape = 16)),
      shape = gg$guide_legend(override.aes = list(color = hdx_signals_palette$map_label))
    )
}
