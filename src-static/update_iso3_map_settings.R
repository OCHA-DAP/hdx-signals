#' Simple script to manually create map dimensions and settings
#'
#' Allows us to manually set all map dimensions for ISO3 codes so they look good
#' when saved. Aim is to have plot somewhere around 3 inches minimum, 5 inches
#' maximum. Currently only working on `height` and `width`.

box::use(dplyr)
box::use(purrr)
box::use(tidyr)
box::use(sf)
box::use(logger[log_info])

box::use(../src/utils/get_iso3_sf)
box::use(../src/utils/all_iso3_codes)
box::use(cs = ../src/utils/cloud_storage)


log_info("Updating map settings...")

#' Gets width to height ratio for base adm0 of iso3
iso3_map_ratio <- function(iso3) {
  sf_obj <- get_iso3_sf$get_iso3_sf(iso3)
  bbox <- sf$st_bbox(sf_obj)
  (bbox[["xmax"]] - bbox[["xmin"]]) / (bbox[["ymax"]] - bbox[["ymin"]])
}

#' Gets the width and height of the map from the map ratio
#'
#' Just produces plots with equal area. Will likely need adjustments for specific
#' locations.
iso3_width_height <- function(iso3) {
  ratio <- iso3_map_ratio(iso3)
  height <- max(min(sqrt(24 / ratio), 6), 4) # ensure the ratios are bound to be 6x4 as most lopsided
  width <- 24 / height
  data.frame(width, height, ratio)
}

# get the widths and heights for all locations
iso3_codes <- all_iso3_codes$all_iso3_codes()

df_width_height <- purrr$map(
  .x = iso3_codes,
  .f = iso3_width_height,
  .progress = interactive()
) |>
  purrr$list_rbind()

df_width_height$iso3 <- iso3_codes


df_map_settings <- df_width_height |>
  dplyr$mutate(
    legend_position = ifelse(ratio > 1.5, "bottom", "left"),
    direction = ifelse(ratio > 1.5, "horizontal", "vertical"),
    justification = ifelse(ratio > 1.5, 0.1, 0.9),
    location = "plot"
  )

if (any(is.na(df_map_settings))) {
  stop(
    "Data missing from `df_map_settings`, fix before trying again.",
    call. = FALSE
  )
}

fname <- "input/iso3_map_settings.json"
cs$update_az_file(
  df = df_map_settings,
  name = fname
)

log_info(paste0("Successfully downloaded map settings and saved to ", fname))
