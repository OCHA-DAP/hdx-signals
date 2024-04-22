box::use(gg = ggplot2)
box::use(scales)

box::use(./theme_signals)

#' Plot time series data
#'
#' Plots time series data. Requires date column and value to be passed in, which
#' will be visualized as a line plot. Adds in the basic Signals theme as well.
#'
#' @param df Data frame to plot
#' @param date Date column to plot
#' @param values Values column to plot
#' @param y_axis Title for the Y axis
#' @param title Title for the plot
#' @param subtitle Subtitle for the plot
#' @param caption Caption for the plot
#'
#' @returns Plot of cholera for that country.
#'
#' @export
plot_ts <- function(
    df, date, values, y_axis, title, subtitle = gg$waiver(), caption = gg$waiver()
) {
  df |>
    gg$ggplot() +
    gg$geom_line(
      gg$aes(
        x = .data[[date]],
        y = .data[[values]]
      )
    ) +
    gghdx$scale_y_continuous_hdx(
      labels = scales$label_comma(),
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
