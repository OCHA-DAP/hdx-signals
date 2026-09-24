box::use(src/images/plots/hdx_signals_palette)

#' Direction colours shared by the SEA5 anomaly plot and map
#'
#' Dry anomalies use the HDX warning (orange) scale and wet anomalies the HDX
#' primary (blue) scale, at matching depths (steps 3, 5 and 7) so neither
#' direction reads as more severe. The plot uses the middle step; the map uses
#' all three, light to dark, for increasing return period classes.
#'
#' @export
direction_palettes <- list(
  dry = c(
    hdx_signals_palette$warning_orange_light,
    hdx_signals_palette$warning_orange,
    hdx_signals_palette$warning_orange_dark
  ),
  wet = c(
    hdx_signals_palette$primary_blue_light,
    hdx_signals_palette$primary_blue,
    hdx_signals_palette$primary_blue_dark
  )
)

#' @rdname direction_palettes
#' @export
direction_colors <- vapply(direction_palettes, `[`, character(1), 2)
