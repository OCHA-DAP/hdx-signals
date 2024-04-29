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
box::use(cs = ../src/utils/cloud_storage)

#' Gets width to height ratio for base adm0 of iso3
iso3_map_ratio <- function(iso3) {
  sf_obj <- get_iso3_sf$get_iso3_sf(iso3)
  bbox <- sf$st_bbox(sf_obj)
  (bbox[["xmax"]] - bbox[["xmin"]]) / (bbox[["ymax"]] - bbox[["ymin"]])
}

#' Gets the width and height of the map from the map ratio
#'
#' Just produces plots with equal area. Will likely need adjustments for specific
#' countries.
iso3_width_height <- function(iso3) {
  ratio <- iso3_map_ratio(iso3)
  height <- max(min(sqrt(24 / ratio), 8), 3) # ensure the ratios are bound to be 8x3 as most lopsided
  width <- 24 / height
  data.frame(width, height)
}

# get the widths and heights for all countries
iso3_codes <- all_iso3_codes$all_iso3_codes()

df_width_height <- purrr$map(
  .x = iso3_codes,
  .f = iso3_width_height
) |>
  purrr$list_rbind()

df_width_height$iso3 <- iso3_codes

cs$update_az_file(
  df = df_width_height,
  name = "input/iso3_width_height.parquet"
)
