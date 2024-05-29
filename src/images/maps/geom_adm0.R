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
#' @param ... Additional sf class objects with geometry
#'
#' @returns Geom for the country boundaries
#'
#' @export
geom_adm0 <- function(iso3, ...) {
  sf_adm0 <- get_iso3_sf$get_iso3_sf(iso3, "adm0")

  if (is.null(sf_adm0) || nrow(sf_adm0) == 0) {
    stop(
      "No country boundaries data for ",
      iso3,
      ". Ensure that the country is handled in `get_adm0_sf()`.",
      call. = FALSE
    )
  }

  additional_geoms <- list(...)
  if (length(additional_geoms) > 0) {
    purrr$walk(
      .x = additional_geoms,
      .f = \(x) assert_covered_by(x, sf_adm0)
    )
  }

  gg$geom_sf(data = sf_adm0)
}

#' Assert that `x` is contained within `y`.
#'
#' Applies a buffer of `dist` meters to polygon `y` and checks if all
#' elements of `x` are within buffered polygon `y`. If not, an error is returned.
#'
#' @param x sf class with geometry feature that is being validated against y polygon
#' @param y sf class POLYGON/MULTIPOLYGON to use to check if all x falls within
#' @param dist distance in meters (default = 10000)
#'
#' @return TRUE if all elements of x fall within `dist` distance of y.
#'     Return error if any elements fall outside of distance
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
#'  assert_covered_by(
#'    x = pts_sampled,
#'    y =alleghany_county,
#'    dist = 1000
#'    )
#'  assert_covered_by(
#'    x= pts_sampled_bbox,
#'    y = alleghany_county,
#'    dist = 1000
#'    )
#'  clay <- filter(nc_counties, NAME == "Clay")
#'  macon <- filter(nc_counties, NAME == "Macon")
#'  assert_covered_by(clay, macon,dist=1000)

assert_covered_by <- function(x, y, dist = 10000) {
  y_buff <- sf$st_buffer(
    y,
    dist = units::set_units(dist, metres)
  )
  lgl_covers <- sf$st_covered_by(
    x = x,
    y = y_buff,
    sparse = FALSE
  )
  not_within <- any(!lgl_covers)

  if (not_within) {
    idx_not_within <- which(!lgl_covers)

    stop(
      "Error: elements in x not contained within ", dist, "m of base map boundary:\n",
      paste(idx_not_within, sep = "=", collapse = ", "),
      "."
    )
  } else {
    return(TRUE)
  }
}
