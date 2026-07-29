box::use(
  gg = ggplot2,
  scales,
  gghdx,
  stringr,
  dplyr
)

box::use(
  src/images/maps/sf_adm0,
  src/images/maps/geom_cities,
  src/images/maps/map_theme,
  src/images/plots/hdx_signals_palette,
  cs = src/utils/cloud_storage
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
  map_width <- get_map_width(iso3 = iso3, use_map_settings = use_map_settings)
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
      stroke = 1
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
      # 12px captions (per the style guide) are wider than the fixed map
      # canvas for longer strings, so wrap proportionally to this map's
      # actual width - without this, text renders past the device edge and
      # gets cropped on export
      caption = wrap_caption(caption, map_width)
    ) +
    map_theme$map_theme(
      iso3 = iso3,
      use_map_settings = use_map_settings
    ) +
    gg$theme(
      panel.border = gg$element_blank(),
      panel.background = gg$element_blank(),
      # keep the title on one line: shrink it to a size that fits this map's
      # actual width instead of wrapping, since a wrapped title eats into the
      # vertical space the map itself has to render in
      plot.title = gg$element_text(size = title_size(map_width))
    )
}

#' Get the export width (in inches) for an ISO3 map
#'
#' Mirrors the width `save_map()` will use when actually exporting the image,
#' so title/caption sizing can be tuned to the map's real canvas rather than
#' assuming a flat size. Falls back to `create_images()`'s own default width
#' when not using the real per-country settings.
#'
#' @param iso3 ISO3 code
#' @param use_map_settings Whether or not to look up real per-country settings
#'
#' @returns Map width, in inches
get_map_width <- function(iso3, use_map_settings) {
  if (use_map_settings) {
    cs$read_az_file_cached("input/iso3_map_settings.json") |>
      dplyr$filter(iso3 == !!iso3) |>
      dplyr$pull(width)
  } else {
    6
  }
}

#' Title size (in points) that fits this map's width on one line
#'
#' Scaled linearly between the sizes empirically verified - with a real
#' caption rendered alongside, against the real per-country legend settings,
#' not just title or caption tested in isolation, since the two compete for
#' the same gtable column width - to keep a realistic long map title (e.g.
#' "Reported events since 14 February 2026") on a single line at the
#' narrowest (4in) and widest (6in) map canvases used across
#' `input/iso3_map_settings.json`.
#'
#' @param map_width Map width, in inches
#'
#' @returns Title font size, in points
title_size <- function(map_width) {
  size <- 12 + (map_width - 4)
  max(12, min(14, size))
}

#' Wrap each line of a caption to fit the map's actual canvas width
#'
#' Wraps line-by-line rather than as a whole paragraph, since captions already
#' arrive with intentional line breaks (source, location/date, UN disclaimer)
#' that should stay separate rather than being reflowed together. The
#' chars-per-inch figure (8) is empirically calibrated - with a real title
#' rendered alongside, against the real per-country legend settings - against
#' the widest and narrowest map sizes in `input/iso3_map_settings.json`.
#'
#' @param caption Caption text, or `ggplot2::waiver()` if none was passed
#' @param map_width Map width, in inches
#'
#' @returns Wrapped caption text, or `caption` unchanged if it is a `waiver`
wrap_caption <- function(caption, map_width) {
  if (inherits(caption, "waiver")) {
    caption
  } else {
    wrap_width <- round(8 * map_width)
    paste(
      vapply(
        strsplit(caption, "\n")[[1]],
        \(line) stringr$str_wrap(line, width = wrap_width),
        character(1)
      ),
      collapse = "\n"
    )
  }
}
