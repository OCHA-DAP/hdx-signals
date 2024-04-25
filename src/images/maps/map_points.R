box::use(gg = ggplot2)
box::use(scales)
box::use(gghdx)
box::use(lubridate)

box::use(../plots/theme_signals)
box::use(./gg_map)
box::use(./geom_cities)

#' Map points data
#'
#' Maps points data.
#'
#' @param iso3 ISO3 code used to get base plot and cities
#' @param df Points sf to plot
#' @param val_col Values column to use for size
#' @param size Title for the size legend
#' @param title Title for the plot
#' @param subtitle Subtitle for the plot
#' @param caption Caption for the plot
#'
#' @returns Map plot
#'
#' @export
map_points <- function(
    iso3, df, val_col, size, title, subtitle = gg$waiver(), caption = gg$waiver()
) {
  gg_map$gg_map(iso3) +
    gg$geom_sf(
      data = df,
      mapping = gg$aes(
        size = .data[[val_col]]
      ),
      color = gghdx$hdx_hex("tomato-hdx"),
      alpha = 0.75
    ) +
    geom_cities$geom_cities(iso3) +
    gg$scale_size_continuous(
      breaks = scales$pretty_breaks(),
      labels = scales$label_comma()
    ) +
    gg$coord_sf(
      clip = "off",
      crs = "OGC:CRS84"
    ) +
    gg$labs(
      x = "",
      y = "",
      size = size,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    gg$theme(
      rect = gg$element_blank(),
      axis.ticks = gg$element_blank(),
      axis.text.x = gg$element_blank(),
      axis.text.y = gg$element_blank(),
      panel.grid.major = gg$element_blank(),
      panel.grid.minor = gg$element_blank(),
      axis.line.x = gg$element_blank(),
      plot.caption = gg$element_text(hjust = 1),
      legend.position = "left",
      legend.direction = "vertical"
    )
}
