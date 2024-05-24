box::use(gg = ggplot2)
box::use(dplyr)
box::use(purrr)
box::use(sf)
box::use(units)

box::use(../../utils/get_iso3_sf)
box::use(../../utils/iso3_shift_longitude)


#' Geom for country boundaries
#'
#' Adds country boundaries for ISO3 code. Since `get_adm0_sf()` looks for custom
#' basemaps, then OCHA CODs, then UN Geodata services by filtering, we throw
#' an error if the returned data is `NULL` or a 0 row data frame.
#'
#' @param iso3 ISO3 code
#' @param additional_geom sf class object with geometry (default = NULL)
#'
#' @returns Geom for the country boundaries
#'
#' @export
geom_adm0 <- function(iso3, additional_geom = NULL) {
  sf_adm0 <- get_iso3_sf$get_iso3_sf(iso3, "adm0")

  if (!null(other_geom)) {
    assert_within_distance(additional_geom, sf_adm)
  }

  if (is.null(sf_adm0) || nrow(sf_adm0) == 0) {
    stop(
      "No country boundaries data for ",
      iso3,
      ". Ensure that the country is handled in `get_adm0_sf()`.",
      call. = FALSE
    )
  }

  gg$geom_sf(data = sf_adm0)
}




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

  # cast to points so a polygon w/ vertices outside `y` will create error
  x <- sf$st_cast(x,"POINT")

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
