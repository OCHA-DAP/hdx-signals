box::use(sf)

#' Shift longitude if necessary, based on ISO3
#'
#' Since countries spanning the prime meridian require shifting longitude to plot
#' correctly, so that the values range 0 to 360, we need to do this for basemap
#' and any other shapes for those countries. We do this by just checking if it's
#' needed for that ISO3 code.
#'
#' @param sf_obj Shapefile
#' @param iso3 ISO3 code
#'
#' @export
iso3_shift_longitude <- function(sf_obj, iso3) {
  shift_iso3s <- c("FJI", "KIR", "NZL", "RUS")
  if (iso3 %in% shift_iso3s) {
    sf_obj <- sf$st_shift_longitude(sf_obj)
  }
  sf_obj
}
