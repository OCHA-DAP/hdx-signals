box::use(dplyr)
box::use(purrr)
box::use(sf)
box::use(units)

#' assert_within_distance
#' Assert that `x` is within `dist` distance from `y`.
#' @param x sf class with geometry
#' @param y sf class with geometry
#' @param dist distance in meters
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
#'    y =alleghany_county,
#'    dist = 1000
#'    )


assert_within_distance <- function(x,y, dist=10000){

  m_within_dist <- sf$st_is_within_distance(
    x = x,
    y = y,
    dist = units$set_units(dist,meters),
    sparse = F
  )

  not_within <- any(!m_within_dist)
  if (not_within) {
    idx_not_within <- which(!m_within_dist)

    stop(
      "Error: elements in x not contained within ",dist, "m of y:\n",
      paste(idx_not_within, sep = "=", collapse = ", "),
      ".") } else {
        return(TRUE)
      }
}


# readr$write_rds(x = bgd_adm0,"bgd_adm0.rds")
#' assert_in_buffered_geom
#'
#' @param geom sf geometry. In this use case it will likely always be the admin 0 boundary
#'     returned from get_iso3_sf$get_iso3_sf(iso3, "adm0")
#' @param geom_buffer_pct choose an approximate % to buffered each polygon element of the geom by
#' @param x 2nd geom to test whether it fits entirely in the geom after being buffered
#'
#' @return
#' @export
#'
#' @examples
assert_in_buffered_geom <- function(
    geom,
    geom_buffer_pct,
    x
){
  geom_buffered <- buffer_by_pct(
    geom= geom,
    pct =  geom_buffer_pct
  )
  # pseudo code begins
  # sf$st_within(x, geom_buffered)
  # if not all x within geom_buffered
  # create alert
}


#' buffer_by_pct
#' given a POLYGON or MULTIPOLYGON buffer each each polygon by a % of the length
#' of it's bbox hypotenous
#' @param x POLYGON or MULTIPOLYGON
#' @param pct `numeric` percentage to buffer by
#'
#' @return POLYGON or MULTIPOLYGON
#' @examples
#' bgd_adm0 <- get_iso3_sf$get_iso3_sf("BGD", "adm0")
#' buffer_by_pct(bgd_adm0,0.1)

buffer_by_pct <-  function(geom,pct=0.1){

  conversion_factor <- 111132  # meters per degree (approximately)

  geom_cast <- sf$st_cast(geom,to = "POLYGON")
  geom_diag_length <- sqrt(sf$st_area(geom_cast))/conversion_factor

  geom_cast <- geom_cast |>
    dplyr$mutate(
      diag_approx = as.numeric(geom_diag_length),
      buffer_dist = diag_approx * pct
    )

  geom_cast_buffered <- geom_cast |>
    dplyr$transmute(
      buffer_geom = sf$st_buffer(x = geom_cast$geometry, dist = buffer_dist)
    )

  # obj has two geometries so we should set the recognized geometry
  # to the buffered shapes and drop the other one
  # then we dissolve
  geom_cast_buffered |>
    sf$st_set_geometry("buffer_geom") |>
    dplyr$select(-geometry) |>
    dplyr$rename(
      geometry = "buffer_geom"
    ) |>
    dplyr$summarise()
}



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


#' @examples
#'  poly_1 <- sf$st_polygon(
#'    list(
#'      cbind(
#'        c(12.185373, 12.255774, 12.728886, 12.947403, 12.672345, 12.635662, 12.185373),
#'        c(12.529575, 11.939943, 11.967415, 12.21874, 12.435015, 12.558468, 12.529575)
#'      )
#'    )
#'  )
#'  poly_2 <- st_polygon(
#'    list(
#'      cbind(
#'        c(12.240334, 12.26224, 12.913666, 12.240334),
#'        c(12.510266, 12.021816, 12.527339, 12.510266)
#'      )
#'    )
#'  ) |>
#'    st_sfc(crs=4326)
#'
#'  assert_geom_containment(
#'    x= poly_1,
#'    y=pts_1
#'    )
#'
#'  assert_geom_containment(
#'    x= poly_2,
#'    y=pts_1
#'    )
#'
#'  assert_geom_containment(x = pts_1,
#'                          y = pts_2
#'                          )


assert_geom_containment <- function(x,y) {
  x_bbox <- sf$st_bbox(x)
  y_bbox <- sf$st_bbox(y)

  x_bbox_contains_ybbox <-
    y_bbox$xmin >= x_bbox$xmin &&
    y_bbox$xmax <= x_bbox$xmax &&
    y_bbox$ymin >= x_bbox$ymin &&
    y_bbox$ymax <= x_bbox$ymax

  if (!(
    x_bbox_contains_ybbox
  )) {
    stop("The bounding box of the provided sf object provided as y is not contained within the bounding box of sf object provided as x.",
         call. = FALSE)
  }
  else {
    return(TRUE)
  }

}
