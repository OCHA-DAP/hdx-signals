box::use(sf)

sf$sf_use_s2(FALSE)

#' Crop with adjusted bbox
#'
#' Gets the bbox for an `sf` object, and then adjusts it based on the parameters
#' based in, then crops the `sf`.
#'
#' @param sf_obj Simple feature object
#' @param xmin Amount to adjust xmin
#' @param xmax Amount to adjust xmax
#' @param ymin Amount to adjust ymin
#' @param ymax Amount to adjust ymax
#'
#' @returns Cropped simple feature
#'
#' @export
st_crop_adj_bbox <- function(sf_obj, xmin = 0, xmax = 0, ymin = 0, ymax = 0) {
  old_bbox <- sf$st_bbox(sf_obj)
  sf$st_crop(
    sf_obj,
    xmin = old_bbox[["xmin"]] + xmin,
    xmax = old_bbox[["xmax"]] + xmax,
    ymin = old_bbox[["ymin"]] + ymin,
    ymax = old_bbox[["ymax"]] + ymax
  )
}
