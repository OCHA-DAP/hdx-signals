#' Simple script to manually create map dimensions and settings
#'
#' Allows us to manually set all map dimensions for ISO3 codes so they look good
#' when saved. Aim is to have plot somewhere around 3 inches minimum, 5 inches
#' maximum. Currently only working on `height` and `width`.

box::use(dplyr)
box::use(purrr)
box::use(sf)

box::use(../src/utils/get_iso3_sf)
box::use(../src/utils/all_iso3_codes)

#' Gets width to height ratio for base adm0 of iso3
get_map_ratio <- function(iso3) {
  sf_obj <- get_iso3_sf$get_iso3_sf(iso3)
  bbox <- sf$st_bbox(sf_obj)
  (bbox[["xmax"]] - bbox[["xmin"]]) / (bbox[["ymax"]] - bbox[["ymin"]])
}
