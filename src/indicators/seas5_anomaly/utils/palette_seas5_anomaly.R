box::use(
  src/images/plots/hdx_signals_palette,
  src/indicators/seas5_anomaly/utils/alert_seas5_anomaly
)

#' Return period classes and colours shared by the SEAS5 anomaly plot and map
#'
#' Return periods at or above the signal threshold fall in three classes, light
#' to dark. Dry anomalies use the HDX warning (orange) scale and wet anomalies
#' the HDX primary (blue) scale, at matching depths (steps 3, 5 and 7) so
#' neither direction reads as more severe. Return periods below the threshold
#' share the neutral `below_threshold_fill`.
#'
#' @export
rp_breaks <- c(alert_seas5_anomaly$seas5_rp_years, 10, 20, Inf)

#' @rdname rp_breaks
#' @export
rp_labels <- c("5 to 10 years", "10 to 20 years", "20 years or more")

#' @rdname rp_breaks
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

#' @rdname rp_breaks
#' @export
below_threshold_fill <- hdx_signals_palette$map_fill

#' Classify return periods
#'
#' @param rp Return period in years
#'
#' @returns Factor with levels `rp_labels`, `NA` below the signal threshold
#'
#' @export
rp_class <- function(rp) {
  cut(rp, breaks = rp_breaks, labels = rp_labels, right = FALSE)
}
