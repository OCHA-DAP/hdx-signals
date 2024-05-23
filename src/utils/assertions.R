box::use(dplyr)
box::use(purrr)
box::use(sf)
box::use(units)

#' assert_within_distance
#' Assert that `x` is within `dist` distance from `y`.
#' @param x sf class with geometry
#' @param y sf class with geometry
#' @param dist distance in meters (default = 10000)
#'
#' @return TRUE if all elements of x fall within `dist` distance of y.
#'     Return error if any elements fall outside of distance
#' @export
#'
#' @examples
#' library(sf)
#' library(dplyr)
#' library(units)
#' file_name <- system.file("shape/nc.shp", package="sf")
#' nc_counties <- st_read(file_name)
#' alleghany_county <- nc_counties |>
#'     filter(NAME == "Alleghany")
#' # sample points for teseting
#' set.seed(1)
#' # these will all be inside the polygon
#' pts_sampled <- st_sample(
#'    x = alleghany_county,
#'    size = 500
#'    )
#' # these should have some falling outside
#' # since we are sampling the bbox some should be outside
#'  pts_sampled_bbox <- st_sample(
#'    x = st_bbox(alleghany_county) |>
#'      st_as_sfc(),
#'    size = 500
#'    )
#'
#'  assert_within_distance(
#'    x = pts_sampled,
#'    y =alleghany_county,
#'    dist = 1000
#'    )
#'  assert_within_distance(
#'    pts_sampled_bbox,
#'    y = alleghany_county,
#'    dist = 1000
#'    )
assert_within_distance <- function(x, y, dist = 10000) {
  m_within_dist <- sf$st_is_within_distance(
    x = x,
    y = y,
    dist = units$set_units(dist, meters),
    sparse = FALSE
  )

  not_within <- any(!m_within_dist)

  if (not_within) {
    idx_not_within <- which(!m_within_dist)

    stop(
         "Error: elements in x not contained within ", dist, "m of y:\n",
         paste(idx_not_within, sep = "=", collapse = ", "),
         ".")
  } else {
    return(TRUE)
  }
}
