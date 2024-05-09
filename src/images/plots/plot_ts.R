box::use(gg = ggplot2)
box::use(scales)
box::use(gghdx)
box::use(dplyr)

box::use(./theme_signals)
box::use(../../utils/formatters)

#' Plot time series data
#'
#' Plots time series data. Requires date column and value to be passed in, which
#' will be visualized as a line plot. Adds in the basic Signals theme as well.
#'
#' @param df Data frame to plot
#' @param val_col Values column to plot
#' @param y_axis Title for the Y axis
#' @param title Title for the plot
#' @param subtitle Subtitle for the plot
#' @param caption Caption for the plot
#'
#' @returns Time series plot
#'
#' @export
plot_ts <- function(
    df, val_col, y_axis, title, subtitle = gg$waiver(), caption = gg$waiver()
) {
  df |>
    gg$ggplot(
      mapping = gg$aes(
        x = date,
        y = .data[[val_col]]
      )
    ) +
    gg$geom_line(
      linewidth = 0.8,
      color = gghdx$hdx_hex("sapphire-hdx")
    ) +
    gg$geom_point(
      data = dplyr$filter(df, date == max(date)),
      size = 3,
      color = gghdx$hdx_hex("sapphire-hdx")
    ) +
    gghdx$scale_y_continuous_hdx(
      labels = formatters$format_key_figures,
    ) +
    gg$scale_x_date(
      breaks = scales$pretty_breaks(),
      labels = scales$label_date_short()
    ) +
    gg$coord_cartesian(
      clip = "off"
    ) +
    theme_signals$theme_signals() +
    gg$labs(
      x = "",
      y = y_axis,
      title = title,
      subtitle = subtitle,
      caption = caption
    )
}
