box::use(
  gg = ggplot2,
  scales,
  gghdx,
  dplyr
)

box::use(
  src/images/plots/theme_signals,
  src/images/plots/breaks_date
)

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
    df,
    val_col,
    y_axis,
    title,
    alerts,
    subtitle = gg$waiver(),
    caption = gg$waiver()) {
  # data frame for plotting that is filtered to only 5 years of data at most
  plot_df <- df |>
    dplyr$filter(
      max(date, as.Date("1500-01-01")) - date <= 365 * 5
    )


  p <- plot_df |>
    gg$ggplot(
      mapping = gg$aes(
        x = date,
        y = .data[[val_col]]
      )
    ) +
    gg$geom_line(
      linewidth = 0.7,
      color = gghdx$hdx_hex("sapphire-hdx")
    )

  p <- p + gg$geom_point(
    data = dplyr$filter(df, date == max(date, as.Date("1500-01-01"))),
    size = 3,
    color = gghdx$hdx_hex("sapphire-hdx")
  )
  p <- p +
    gghdx$scale_y_continuous_hdx(
      labels = gghdx$label_number_hdx(),
    ) +
    gg$scale_x_date(
      breaks = breaks_date$breaks_date,
      labels = scales$label_date_short()
    ) +
    gg$coord_cartesian(
      clip = "off"
    ) +
    gg$expand_limits(
      y = 0
    ) +
    gg$labs(
      x = "",
      y = y_axis,
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  # determine where to place the margin
  margin_location <- if (inherits(subtitle, "waiver")) "title" else "subtitle"
  p +
    theme_signals$theme_signals(
      margin_location = margin_location,
      x_axis_ticks = TRUE
    ) +
    gg$theme(
      axis.ticks.x.bottom = gg$element_line(
        colour = gghdx$hdx_hex("gray-dark"),
        linewidth = gg$rel(1)
      ),
      axis.ticks.length = gg$unit(-0.05, "in"),
    )

}
