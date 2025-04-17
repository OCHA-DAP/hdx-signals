box::use(
         dplyr,
         gg = ggplot2)

box::use(
  src/images/plots/theme_signals,
  src/images/plots/caption,
  src/images/create_images
)

#' Plot ACAPS INFORM index
#'
#' Creates time series of INFORM index
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
      title = paste0("Inform Severity Index - ", format(as.Date(df_alerts$date), "%B %Y"), ": ", df_alerts$value
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = inform_ts,
    image_use = "plot",
    settings = "plot"
  )
}

#' Plot ACAPS INFORM index data and sub-indicators.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'      series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
inform_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- caption$caption(
    indicator_id = "acaps_inform_severity",
    iso3 = unique(df_wrangled$iso3)
  )

  # only plot the points that are comparable to the latest
  df_plot <- df_wrangled |>
    dplyr$filter(country_level == "Yes")
  df_plot$date <- as.Date(df_plot$date)

  # Determine dynamic y-axis minimum
  y_min <- floor(min(df_plot$inform_severity_index, na.rm = TRUE) - 1)

  # Extract min, max, and latest points for annotation
  min_point <- df_plot[which.min(df_plot$inform_severity_index), ]
  max_point <- df_plot[which.max(df_plot$inform_severity_index), ]
  latest_point <- df_plot[nrow(df_plot), ]

  # Create the plot
  gg$ggplot(df_plot, gg$aes(x = date)) +
    # Background shading for High Severity range
    gg$geom_ribbon(gg$aes(ymin = 4, ymax = 5), fill = "#FDEFEF", alpha = 0.5) +

    # Inform Severity Index line
    gg$geom_line(gg$aes(y = inform_severity_index), color = "#5D3779", size = 1.5) +

    # Add points for min, max, and latest data points
    gg$geom_point(data = min_point, gg$aes(x = date, y = inform_severity_index), color = "#5D3779", size = 3) +
    gg$geom_point(data = max_point, gg$aes(x = date, y = inform_severity_index), color = "#5D3779", size = 3) +
    gg$geom_point(data = latest_point, gg$aes(x = date, y = inform_severity_index), color = "#5D3779", size = 3) +

    # Add labels for min, max, and latest data points
    gg$geom_text(data = min_point, gg$aes(x = date, y = inform_severity_index, label = round(inform_severity_index, 2)),
                 vjust = 1.8, color = "#5D3779", size = 3.5, fontface = "bold") +
    gg$geom_text(data = max_point, gg$aes(x = date, y = inform_severity_index, label = round(inform_severity_index, 2)),
                 vjust = -1, color = "#5D3779", size = 3.5, fontface = "bold") +
    gg$geom_text(data = latest_point,
                 gg$aes(x = date, y = inform_severity_index, label = round(inform_severity_index, 2)),
                 vjust = -1, color = "#5D3779", size = 3.5, fontface = "bold") +

    # Axis and title labels
    gg$labs(
      y = "Index Value",
      title = title
    ) +

    # Customize x-axis for one tick per year
    gg$scale_x_date(date_breaks = "1 year", date_labels = "%Y") +

    # Set y-axis limits dynamically
    gg$coord_cartesian(ylim = c(y_min, 5)) +
    gg$scale_y_continuous(expand = c(0, 0)) +

    # Custom theme
    theme_signals$theme_signals(
      x_axis_ticks = TRUE
    ) +
    gg$theme(
      legend.position = "none",        # Remove legend
      axis.ticks.length.x = gg$unit(5, "pt"),
      axis.text.x = gg$element_text(margin = gg$margin(t = 5)),
    )

}
