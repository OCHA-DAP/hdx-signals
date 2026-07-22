box::use(
  dplyr,
  forcats,
  gg = ggplot2,
  lubridate
)

box::use(
  src/images/plots/theme_signals,
  src/images/create_images,
  src/images/plots/caption,
  src/images/plots/hdx_signals_palette
)

#' Plot JRC ASAP
#'
#' Creates time series of JRC ASAP
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  # add title for use in the plot
  df_plot <- df_alerts |>
    dplyr$mutate(
      title = paste(
        ifelse(
          value == 1,
          "Hotspot",
          "Major hotspot"
        ),
        "declared",
        format(date, "%B %Y")
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = hotspots_ts,
    image_use = "plot",
    height = 3,
    width = 6
  )
}

#' Plot JRC drought data
#'
#' Plots JRC drought data for a specific location.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Used to draw a focus ring around the
#'     declared month.
#'
#' @returns Plot of cholera for that wrangled data
hotspots_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- caption$caption(
    indicator_id = "jrc_agricultural_hotspots",
    iso3 = unique(df_wrangled$iso3)
  )

  df_plot <- df_wrangled |>
    dplyr$mutate(
      year = lubridate$year(date),
      month = lubridate$month(date, label = TRUE),
      hs_name = forcats$fct_expand(hs_name, "Major hotspot", "Hotspot", "No hotspot"),
      hs_name = forcats$fct_relevel(hs_name, "Major hotspot", "Hotspot", "No hotspot")
    ) |>
    dplyr$filter(
      max(year, -Inf) - year < 5
    )

  declared <- dplyr$filter(
    df_plot,
    year == lubridate$year(date),
    month == lubridate$month(date, label = TRUE)
  )

  df_plot |>
    gg$ggplot(
      mapping = gg$aes(
        x = month,
        y = year,
        fill = hs_name
      )
    ) +
    gg$geom_tile(
      color = hdx_signals_palette$hairline
    ) +
    # focus ring calling out the month the hotspot/alert was declared
    gg$geom_tile(
      data = declared,
      fill = NA,
      color = hdx_signals_palette$primary_blue,
      linewidth = 0.8
    ) +
    gg$scale_x_discrete(
      breaks = c("Jan", "Apr", "Jul", "Oct")
    ) +
    gg$scale_y_reverse(
      breaks = unique(df_plot$year),
    ) +
    theme_signals$theme_signals() +
    gg$theme(
      legend.position = "none"
    ) +
    gg$scale_fill_manual(
      values = c(
        "No hotspot" = "#FFFFFF",
        "Hotspot" = hdx_signals_palette$data_grid_blue,
        "Major hotspot" = hdx_signals_palette$primary_blue_dark
      )
    ) +
    gg$labs(
      title = title,
      caption = caption
    ) +
    gg$coord_equal() +
    gg$guides(
      fill = gg$guide_legend(
        byrow = TRUE
      )
    ) +
    gg$theme(
      legend.title = gg$element_blank(),
      axis.title = gg$element_blank(),
      panel.grid = gg$element_blank(),
      legend.position = "left",
      legend.direction = "vertical",
      legend.spacing.y = gg$unit(x = 0.1, units = "in"),
      axis.line.x = gg$element_blank()
    )
}
