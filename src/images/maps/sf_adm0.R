box::use(
  dplyr,
  purrr,
  rlang,
  sf,
  logger
)

box::use(
  cs = src/utils/cloud_storage,
  src/utils/get_iso3_sf,
  src/utils/iso3_shift_longitude,
  src/utils/st_crop_adj_bbox
)

#' Load location boundaries data
#'
#' Loads location boundaries for ISO3 code. Since `get_adm0_sf()` looks for custom
#' basemaps, then OCHA CODs, then UN Geodata services by filtering. We throw an
#' error if the returned data is `NULL` or a 0 row data frame.
#'
#' If you pass in additional geometries, these are checked against the admin
#' boundaries. By default, an `error` is thrown if the additional geometries fall
#' outside a buffer of the admin area. You can also choose instead to `filter` the
#' geometries, so that anything outside those buffers is removed from the map.
#' This is used for instance, for ACLED, where naval conflicts are occassionally
#' reported but we do not map.
#'
#' For the `error` action, points that fall outside the buffer are given one more
#' chance against the location's full, untrimmed boundary (see
#' `get_iso3_sf_full()`) before being treated as bad data. This matters because
#' `update_adm0_sf()` deliberately trims some locations' display boundary to drop
#' far-flung territory (e.g. Hawaii and Guam for the USA, Easter Island for Chile)
#' so the default map isn't dominated by it. Without this fallback, genuine data
#' in that territory looks identical to genuinely bad data (the wrong country, or
#' the middle of the ocean) and both delete all generated campaign content. Points
#' only covered by the full boundary are real, but outside what the trimmed map
#' shows, so they're dropped from `additional_geoms` instead of erroring.
#'
#' @param iso3 ISO3 code
#' @param action What to do when the additional boundaries fall outside the admin
#'     area
#' @param ... Additional sf class objects with geometry
#'
#' @returns Named list with `sf_adm0` and `additional_geoms` from `...`
#'
#' @export
sf_adm0 <- function(iso3, action = c("error", "filter", "nothing"), ...) {
  action <- rlang$arg_match(action)
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
  if (length(additional_geoms) > 0 && action != "nothing") {
    sf_adm0_buff <- suppressWarnings(
      suppressMessages(
        sf$st_buffer(x = sf_adm0, dist = 0.2)
      )
    )
    if (action == "error") {
      additional_geoms <- reconcile_all(
        x_list = additional_geoms,
        y = sf_adm0_buff,
        iso3 = iso3,
        # wrapped in a thunk and only called if something fails the primary
        # check, since most calls never need the fallback and it costs a read
        # of the (large) world boundary file
        get_full = \() get_iso3_sf_full(iso3)
      )
    } else if (action == "filter") {
      additional_geoms <- intersect_all(
        x_list = additional_geoms,
        y = sf_adm0_buff
      )
    }
  }

  # also check if we need to reduce the administrative areas for some locations
  sf_adm0 <- reduce_adm0(iso3 = iso3, sf_adm0 = sf_adm0, additional_geoms = additional_geoms)

  list(
    sf_adm0 = sf_adm0,
    additional_geoms = additional_geoms
  )
}

#' Reconcile all of `x_list` against `y`, falling back to the full boundary
#'
#' Maps `reconcile_covered_by` across all of `...`, so that each is checked
#' against `y`, then, for anything left over, against the full boundary for
#' `iso3`. See `sf_adm0()` for why the fallback is necessary.
#'
#' @param x_list All geometries to check
#' @param y sf class POLYGON/MULTIPOLYGON, buffered, to check if x falls within
#' @param iso3 ISO3 code, used in the warning/error message if elements are dropped
#'     or rejected
#' @param get_full Zero-argument function returning the full boundary for the
#'     fallback check (or `NULL` if none is available), only called if needed
#'
#' @return `x_list`, with elements only covered by the full boundary dropped.
#'     Stops with an error if any element is covered by neither boundary.
reconcile_all <- function(x_list, y, iso3, get_full) {
  purrr$map(
    .x = x_list,
    .f = \(x) reconcile_covered_by(x = x, y = y, iso3 = iso3, get_full = get_full)
  )
}

#' Check that `x` is covered by `y`, row by row
#'
#' Unlike `assert_covered_by()`, this doesn't collapse `x` to a single geometry
#' first, so it can report which individual rows of `x` fall outside `y`.
#'
#' @param x sf class with geometry feature that is being validated against y polygon
#' @param y sf class POLYGON/MULTIPOLYGON to use to check if x falls within
#'
#' @return Logical vector, one per row of `x`, `TRUE` if that row is covered by `y`.
row_covered_by <- function(x, y) {
  suppressMessages(
    sf$st_covered_by(x = x, y = y, sparse = FALSE)
  ) |>
    apply(1, any)
}

#' Reconcile `x` against `y`, falling back to the full boundary for `iso3`
#'
#' Rows of `x` covered by `y` are kept as-is. Rows not covered by `y` are given
#' one more chance against `get_full()`: if that full, untrimmed boundary covers
#' them, they're genuine data in territory the display map trims out, so they're
#' dropped from what gets plotted (with a warning logged for visibility) instead
#' of failing the whole map. If a row is covered by neither boundary, it's
#' treated as bad data and an error is raised, as before.
#'
#' @param x sf class with geometry feature that is being validated against y polygon
#' @param y sf class POLYGON/MULTIPOLYGON, buffered, to check if x falls within
#' @param iso3 ISO3 code, used in the warning/error message if elements are dropped
#'     or rejected
#' @param get_full Zero-argument function returning the full boundary for the
#'     fallback check (or `NULL` if none is available), only called if needed
#'
#' @return `x`, with any rows only covered by the full boundary dropped.
reconcile_covered_by <- function(x, y, iso3, get_full) {
  covered <- row_covered_by(x, y)
  if (all(covered)) {
    return(x)
  }

  sf_full <- get_full()
  covered_by_full <- !is.null(sf_full) &&
    all(row_covered_by(
      x[!covered, ],
      suppressWarnings(suppressMessages(sf$st_buffer(sf_full, dist = 0.2)))
    ))

  if (!covered_by_full) {
    stop(
      "Error: elements in x not contained within 0.2 degrees of base map boundary.",
      call. = FALSE
    )
  }

  logger$log_warn(
    sum(!covered), " element(s) for ", iso3, " fall within the full location ",
    "boundary but outside the trimmed display map (e.g. a remote territory ",
    "not shown by default). Dropping from the plot instead of erroring."
  )
  x[covered, ]
}

#' Get the full, untrimmed boundary for a location
#'
#' `get_iso3_sf()` returns the display boundary, which `update_adm0_sf()`
#' deliberately trims for some locations (e.g. dropping Hawaii and Guam for the
#' USA, or Easter Island for Chile) so the default map isn't dominated by
#' far-flung territory. This instead reads the untrimmed UN Geodata boundary, used
#' as a fallback so genuine data in that territory isn't mistaken for bad data.
#'
#' @param iso3 ISO3 code
#'
#' @returns sf class object with the full boundary, or `NULL` if `iso3` isn't in
#'     the UN Geodata file (e.g. it uses a custom, non-standard boundary source).
get_iso3_sf_full <- function(iso3) {
  sf_full <- cs$read_az_file_cached("input/un_geodata_complex.geojson") |>
    dplyr$filter(ISO3CD == iso3)

  if (nrow(sf_full) == 0) {
    return(NULL)
  }

  sf_full |>
    sf$st_set_agr("constant") |>
    dplyr$summarise(do_union = TRUE) |>
    iso3_shift_longitude$iso3_shift_longitude(iso3)
}

#' Intersect all of `x_list` with `y`
#'
#' Runs `sf::st_intersection()` on each element of `x`, so that it only keeps
#' those elements intersecting `y`.
#'
#' @param x_list All geometries to intersect with `y`
#' @param y sf class POLYGON/MULTIPOLYGON, typically admin boundaries, for
#'     intersection
#'
#' @returns `x_list` where all elements have been intersected with `y`
intersect_all <- function(x_list, y) {
  suppressMessages(
    suppressWarnings(
      purrr$map(
        .x = x_list,
        .f = \(x) sf$st_intersection(x = x, y = y)
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
