box::use(
  gg = ggplot2,
  grDevices,
  grid,
  sf
)

#' Fit a map to the full image canvas
#'
#' A map drawn with a fixed aspect ratio only fills as much of the canvas as its
#' shape allows, and the title, legends and caption are laid out against that
#' narrower panel. For tall maps, such as Afghanistan, the text then starts well
#' inside the canvas and runs off the right edge. This widens the map limits
#' (without distorting the map) so that the panel fills the room left over once
#' the title, legends and caption are accounted for. The map stays centered and
#' the annotations line up with the edge of the canvas.
#'
#' Plots that do not use a longitude/latitude `coord_sf()` are returned as they
#' are.
#'
#' @param p Map plot object
#' @param width Width of the canvas (in inches)
#' @param height Height of the canvas (in inches)
#' @param safety Fraction of the available panel space to fill, leaving a small
#'     buffer for text measurements that differ slightly between devices.
#'
#' @returns Plot object with limits set so the map fills the canvas
#'
#' @export
fit_canvas <- function(p, width, height, safety = 0.98) {
  if (!inherits(p$coordinates, "CoordSf")) {
    return(p)
  }

  panel_params <- gg$ggplot_build(p)$layout$panel_params[[1]]
  if (!isTRUE(sf_is_longlat(panel_params$crs))) {
    return(p)
  }

  # measure text on the same kind of device `ggsave()` writes to, so that fonts
  # registered with showtext are found
  open_png_device(width, height)
  on.exit(grDevices$dev.off(), add = TRUE)
  gtable <- gg$ggplotGrob(p)
  fixed_height <- fixed_size(gtable$heights, grid$convertHeight)
  fixed_width <- fixed_size(gtable$widths, grid$convertWidth)

  target_ratio <- (height - fixed_height) / (width - fixed_width) * safety
  x_range <- panel_params$x_range
  y_range <- panel_params$y_range
  cos_lat <- cos(mean(y_range) * pi / 180)
  current_ratio <- diff(y_range) / diff(x_range) / cos_lat

  # widen whichever axis is too short for the room available
  if (current_ratio > target_ratio) {
    half_width <- diff(y_range) / (target_ratio * cos_lat) / 2
    x_range <- mean(x_range) + c(-half_width, half_width)
  } else {
    half_height <- target_ratio * cos_lat * diff(x_range) / 2
    y_range <- mean(y_range) + c(-half_height, half_height)
  }

  suppressMessages(
    p + gg$coord_sf(
      xlim = x_range,
      ylim = y_range,
      expand = FALSE,
      clip = p$coordinates$clip,
      crs = p$coordinates$crs
    )
  )
}

#' Total fixed size of the non-panel rows or columns of a plot, in inches
#'
#' @param sizes Unit vector of `gtable` heights or widths
#' @param convert Either `grid::convertHeight()` or `grid::convertWidth()`
fixed_size <- function(sizes, convert) {
  is_fixed <- grid$unitType(sizes) != "null"
  sum(convert(sizes[is_fixed], "in", valueOnly = TRUE))
}

#' Whether a CRS is longitude/latitude, `FALSE` if it is missing
#'
#' @param crs CRS object from the panel parameters
sf_is_longlat <- function(crs) {
  !is.null(crs) && isTRUE(sf$st_is_longlat(crs))
}

#' Open a temporary PNG device of the canvas size
#'
#' Uses `ragg` when installed, as `ggplot2::ggsave()` does, and the cairo `png()`
#' device otherwise. The device is closed by the caller.
#'
#' @param width Width of the canvas (in inches)
#' @param height Height of the canvas (in inches)
open_png_device <- function(width, height) {
  path <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(path, width = width, height = height, units = "in", res = 300)
  } else {
    grDevices$png(path, width = width, height = height, units = "in", res = 300, type = "cairo")
  }
}
