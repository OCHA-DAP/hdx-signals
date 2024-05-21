box::use(sf)

#' Assert the bbox of 2 geometries overlap
#'
#' @param x sf class object with geometry
#' @param y sf class object with geometry
#'
#' @return TRUE if bounding boxes of 2 geometries overlap. Returns error if
#'     they do not overlap
#' @export
#'
#' @example
#' library(tibble)
#' library(sf)
#' pts_1 <- tribble(
#'   ~X,        ~Y,
#'   12.391938, 12.238959,
#'   12.50738, 12.034744,
#'   12.617324, 12.335638,
#'   12.441413, 12.464487,
#'   12.820722,  12.16374
#'   ) |>
#'   st_as_sf(coords = c("X","Y"),crs=4326)
#'
#' pts_2 <- tribble(
#'   ~X,        ~Y,
#'   12.787738, 12.577178,
#'   12.694286, 11.932578,
#'   13.172544, 11.943334,
#'   13.16155,  12.40007
#'   ) |>
#'   st_as_sf(coords = c("X","Y"), crs = 4326)
#'
#' pts_3 <- tribble(
#'   ~X,        ~Y,
#'   14, 8
#'  ) |>
#'   st_as_sf(coords = c("X","Y"), crs = 4326)
#'
#' assert_bbox_overlap(pts_1, pts_2)
#' assert_bbox_overlap(pts_1, pts_3)
#' st_intersects(pts_1,pts_2)
#' st_intersects(pts_1,pts_3)
assert_bbox_overlap <- function(x, y) {
  x_bbox <- sf$st_bbox(x)
  y_bbox <- sf$st_bbox(y)

  no_overlap <- x_bbox[["xmax"]] < y_bbox[["xmin"]] ||
    x_bbox[["xmin"]] > y_bbox[["xmax"]] ||
    x_bbox[["ymax"]] < y_bbox[["ymin"]] ||
    x_bbox[["ymin"]] > y_bbox[["ymax"]]

  # Assert that bounding boxes overlap with informative error message
  if (no_overlap) {
    stop("Error: Bounding boxes do not overlap. Bounding box x: ",
         paste(names(x_bbox), x_bbox, sep = "=", collapse = ", "),
         ". Bounding box y: ",
         paste(names(y_bbox), y_bbox, sep = "=", collapse = ", "),
         ".")
  } else {
    return(TRUE)
  }

}
