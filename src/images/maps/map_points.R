box::use(
  gg = ggplot2,
  scales,
  gghdx
)

box::use(
  src/images/maps/sf_adm0,
  src/images/maps/geom_cities,
  src/images/maps/map_theme,
  src/images/plots/hdx_signals_palette
)

#' Map points data
#'
#' Maps points data.
#'
#' @param iso3 ISO3 code used to get base plot and cities
#' @param sf_points Points sf to plot
#' @param val_col Values column to use for size
#' @param size Title for the size legend
#' @param action Passed to `sf_adm0::sf_adm0()`
#' @param subtitle Subtitle for the plot, no title used
#' @param caption Caption for the plot
#' @param settings Whether or not to use map settings in `input/iso3_map_settings.json`
#'     to position the legend.
#' @param symbol_color Hex color for the event symbols. Defaults to the
#'     neutral primary blue; per the HDX dataviz style guide, pass the danger
#'     red token instead when the map is explicitly about fatalities or harm.
#'
#' @returns Map plot
#'
#' @export
map_points <- function(
    iso3,
    sf_points,
    val_col,
    size,
    title,
    action = c("error", "filter", "nothing"),
    subtitle = gg$waiver(),
    caption = gg$waiver(),
    settings = "map",
    symbol_color = hdx_signals_palette$primary_blue) {

  if (settings == "map") {
    use_map_settings <- TRUE
  } else {
    use_map_settings <- FALSE
  }
  num_unique_vals <- length(unique(sf_points[[val_col]]))
  sf_list <- sf_adm0$sf_adm0(
    iso3 = iso3,
    action = action,
    sf_points
  )

  gg$ggplot() +
    gg$geom_sf(
      data = sf_list$sf_adm0,
      fill = hdx_signals_palette$map_fill,
      color = hdx_signals_palette$map_boundary,
      linewidth = 0.3
    ) +
    gg$geom_sf(
      data = sf_list$additional_geoms[[1]],
      mapping = gg$aes(
        size = .data[[val_col]]
      ),
      shape = 21,
      fill = scales$alpha(symbol_color, 0.5),
      color = symbol_color,
      stroke = 0.5
    ) +
    geom_cities$geom_cities(iso3) +
    gg$scale_size_continuous(
      breaks = scales$breaks_pretty(
        n = min(3, num_unique_vals)
      ),
      labels = gghdx$label_number_hdx()
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
    map_theme$map_theme(
      iso3 = iso3,
      use_map_settings = use_map_settings
    ) +
    gg$theme(
      panel.border = gg$element_blank(),
      panel.background = gg$element_blank(),
      #plot.margin = gg$margin(t = 5, r = 30, b = 5, l = 5)
    )
}
