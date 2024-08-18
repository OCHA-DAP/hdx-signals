box::use(gg = ggplot2)
box::use(purrr)
box::use(sf)
box::use(dplyr)

box::use(../../utils/get_iso3_sf)
box::use(../../utils/st_crop_adj_bbox)

#' Geom for location boundaries
#'
#' Adds location boundaries for ISO3 code. Since `get_adm0_sf()` looks for custom
#' basemaps, then OCHA CODs, then UN Geodata services by filtering, we throw
#' an error if the returned data is `NULL` or a 0 row data frame.
#'
#' @param iso3 ISO3 code
#' @param ... Additional sf class objects with geometry
#'
#' @returns Geom for the location boundaries
#'
#' @export
geom_adm0 <- function(iso3, ...) {
  sf_adm0 <- get_iso3_sf$get_iso3_sf(iso3, "adm0")

  if (is.null(sf_adm0) || nrow(sf_adm0) == 0) {
    stop(
      "No location boundaries data for ",
      iso3,
      ". Ensure that the location is handled in `get_adm0_sf()`.",
      call. = FALSE
    )
  }

  additional_geoms <- list(...)

  all_covered <- assert_covered_all(
    x_list = additional_geoms,
    y = sf_adm0
  )

  if (!all_covered) {
    stop(
      "Error: elements in x not contained within 0.3 degrees of base map boundary.",
      call. = FALSE
    )
  }

  # also check if we need to reduce the administrative areas for some locations
  sf_adm0 <- reduce_adm0(iso3 = iso3, sf_adm0 = sf_adm0, additional_geoms = additional_geoms)


  gg$geom_sf(data = sf_adm0)
}

#' Assert all covered
#'
#' Asserts that all geometries in `...` is contained within `y`. Maps
#' `assert_covered_by` across all of `...`. Returns `TRUE` if no geoms passed in.
#'
#' Checks a buffered version of `y`, buffered by `0.3` degrees.
#'
#' @param x_list All geometries to check
#' @param y sf class POLYGON/MULTIPOLYGON to use to check if all x falls within
#'
#' @return TRUE if all of `x_list` fall within `y`.
assert_covered_all <- function(x_list, y) {
  y_buff <- suppressWarnings(
    suppressMessages(
      sf$st_buffer(x = y, dist = 0.3)
    )
  )

  if (length(x_list) > 0) {
    purrr$map_lgl(
      .x = x_list,
      .f = \(x) assert_covered_by(x = x, y = y_buff)
    ) |> all()
  } else {
    TRUE
  }
}


#' Assert that `x` is contained within `y`.
#'
#' Checks if all
#' elements of `x` are within buffered polygon `y`. If not, an error is returned.
#'
#' @param x sf class with geometry feature that is being validated against y polygon
#' @param y sf class POLYGON/MULTIPOLYGON to use to check if all x falls within
#'
#' @return TRUE if all elements of x fall within y.
#'     Return error if any elements fall outside of y.
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
#'    y = alleghany_county,
#'    )
#'  assert_covered_by(
#'    x= pts_sampled_bbox,
#'    y = alleghany_county
#'    )
#'  clay <- filter(nc_counties, NAME == "Clay")
#'  macon <- filter(nc_counties, NAME == "Macon")
#'  assert_covered_by(clay, macon)
assert_covered_by <- function(x, y) {
  x <- suppressMessages(
    dplyr$summarise(x)
  ) # cast to a single multiline/polygon/point

  all(
    suppressMessages(
      sf$st_covered_by(
        x = x,
        y = y,
        sparse = FALSE
      )
    )
  )
}

#' Reduce admin boundaries if possible
#'
#' For specific locations, currently just the USA, our base admin file includes
#' areas that are not necessary for every map. For the USA, for instance, this is
#' Alaska. To drop areas when not unnecessary, we check if any of the geoms passed
#' in as `...` intersect optional areas like Alaska. This will be implemented
#' for additional locations as necessary.
#'
#' @param iso3 ISO3 code
#' @param sf_adm0 Admin0 spatial data
#' @param additional_geoms List of additional geometries to check if they intersect
#'     with `sf_adm0`
#'
#' @returns `sf_adm0`, potentially reduced in size
reduce_adm0 <- function(iso3, sf_adm0, additional_geoms) {
  if (length(additional_geoms) > 0) {
    if (iso3 == "USA") {
      sf_adm0 <- sf$st_set_agr(sf_adm0, "constant")
      alaska <- st_crop_adj_bbox$st_crop_adj_bbox(sf_obj = sf_adm0, ymin = 25)
      in_alaska <- purrr$map_lgl(
        .x = additional_geoms,
        .f = \(x) {
          suppressMessages(
            sf$st_intersects(
              x = x,
              y = alaska,
              sparse = FALSE
            )
          ) |>
            any()
        }
      ) |>
        any() # TRUE if any geometry falls within Alaska

      if (!in_alaska) {
        # drop alaska if not necessary
        sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_obj = sf_adm0, ymax = -20.2)
      }
    }
  }
  sf_adm0
}
