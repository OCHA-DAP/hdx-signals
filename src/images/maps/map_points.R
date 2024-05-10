box::use(gg = ggplot2)
box::use(scales)
box::use(gghdx)
box::use(lubridate)

box::use(../plots/theme_signals)
box::use(./gg_map)
box::use(./geom_cities)
box::use(./map_theme)

#' Map points data
#'
#' Maps points data.
#'
#' @param iso3 ISO3 code used to get base plot and cities
#' @param df Points sf to plot
#' @param val_col Values column to use for size
#' @param size Title for the size legend
#' @param subtitle Subtitle for the plot, no title used
#' @param caption Caption for the plot
#' @param use_map_settings Whether or not to use map settings in `input/iso3_map_settings.json`
#'     to position the legend.
#'
#' @returns Map plot
#'
#' @export
map_points <- function(
    iso3, df, val_col, size, subtitle, caption = gg$waiver(), use_map_settings = TRUE
) {
  gg_map$gg_map(iso3) +
    gg$geom_sf(
      data = df,
      mapping = gg$aes(
        size = .data[[val_col]]
      ),
      color = gghdx$hdx_hex("sapphire-hdx"),
      alpha = 0.8
    ) +
    gg$geom_sf(
      data = df,
      mapping = gg$aes(
        size = .data[[val_col]]
      ),
      color = gghdx$hdx_hex("sapphire-dark"),
      shape = 1,
      stroke = 0.1
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
      subtitle = subtitle,
      caption = caption
    ) +
    map_theme$map_theme(iso3 = iso3, use_map_settings = use_map_settings)
}

