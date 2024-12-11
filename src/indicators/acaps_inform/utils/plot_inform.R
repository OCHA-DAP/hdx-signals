box::use(
  dplyr,
  tidyr,
  scales,
  gg = ggplot2,
  gghdx,
  lubridate,
  readr,
  ggrepel,
  utils,
  glue
)

box::use(
  src/images/plots/theme_signals,
  src/images/plots/caption,
  src/images/plots/breaks_date,
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
      title = paste0(
        scales$label_percent(accuracy = 1)(value),
        " of the ",
        type,
        " analyzed population in P",
        phase_level
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = inform_ts,
    image_use = "plot"
  )
}

#' Plot ACAPS INFORM index data and sub-indicators.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
inform_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- caption$caption(
    indicator_id = "acaps_inform",
    iso3 = unique(df_wrangled$iso3)
  )

  # only plot the points that are comparable to the latest
  df_plot <- df_wrangled

  # Create the plot
  gg$ggplot(df_plot, aes(x = date)) +
    # Background shading using geom_ribbon
    gg$geom_ribbon(aes(ymin = 0, ymax = 3, fill = "Low Severity"), alpha = 0.3) +
    gg$geom_ribbon(aes(ymin = 3, ymax = 4, fill = "Moderate Severity"), alpha = 0.3) +
    gg$geom_ribbon(aes(ymin = 4, ymax = 5, fill = "High Severity"), alpha = 0.3) +


    # Lines for indicators
    gg$geom_line(aes(y = inform_severity_index, color = "Inform Severity Index"), size = 1.6, linetype = "solid") +
    gg$geom_line(aes(y = impact_crisis, color = "Impact Crisis"), size = 1.0, linetype = "dashed") +
    gg$geom_line(aes(y = people_condition, color = "People Condition"), size = 1.0, linetype = "dotted") +
    gg$geom_line(aes(y = complexity, color = "Complexity"), size = 1.0, linetype = "solid") +

    # Define color mappings
    gg$scale_color_manual(
      values = c(
        "Inform Severity Index" = "blue",
        "Impact Crisis" = "cyan",
        "People Condition" = "cyan",
        "Complexity" = "cyan"
      ), breaks = c("Inform Severity Index", "Impact Crisis", "People Condition", "Complexity") # Order for line legend
    ) +
    # Define fill mappings for ribbons with custom grays
    scale_fill_manual(
      values = c(
        "Low Severity" = "#d9d9d9",      # Light gray
        "Moderate Severity" = "#bdbdbd", # Medium gray
        "High Severity" = "#969696"      # Dark gray
      ),
      name = "Severity Level" # Custom title for the ribbon legend
    )

    # Axis and title labels
    gg$labs(
      x = "Date",
      y = "Index Value",
      color = "Indicators",
      title = "Inform Severity Index and sub-indicators"
    ) +

    # Set y-axis limits and ensure clipping isn't done
    gg$coord_cartesian(ylim = c(2, 5)) +

    # Custom theme for styling
    gg$theme_minimal() +
    gg$theme(
      legend.position = "bottom",
      legend.title = gg$element_text(size = 12),
      legend.text = gg$element_text(size = 10),
      plot.title = gg$element_text(size = 14, face = "bold"),
      axis.text.x = gg$element_text(angle = 45, hjust = 1),
      panel.grid.minor = gg$element_blank()  # Remove minor gridlines
    )
}
