box::use(sf)
box::use(dplyr)
box::use(glue)
box::use(purrr)

box::use(./utils/download_shapefile[download_shapefile])
box::use(../src/utils/all_iso3_codes[all_iso3_codes])
box::use(cs = ../src/utils/cloud_storage)

###################
#### FUNCTIONS ####
###################

#' Download and wrangle cities data
#'
#' Downloads cities data from Natural Earth. For a few country codes, we manually
#' create city points that are missing from the file. For AB9 (Abyei), we don't
#' use any cities because of its small size, and for LAC, they are rural regions
#' so we don't add them either.
#'
#' @param iso3 ISO3 country code
#'
#' @returns Shapefile of points for the country
update_cities_sf <- function(iso3) {
  if (iso3 == "XKX") {
    cities_sf <- create_point_sf(lat = 42.667542, lon = 21.166191, name = "Pristina")
  } else if (iso3 == "NRU") {
    cities_sf <- create_point_sf(lat = -0.5456, lon = 166.9157, name = "Yaren")
  } else {
    cities_sf <- dplyr$filter(
      pop_sf,
      adm0_a3 == iso3
    )
  }

  if (nrow(cities_sf) > 0) {
    cs$update_az_file(
      df = cities_sf,
      name = glue$glue("input/cities/{iso3}.geojson")
    )
  }
}

#' Create point simple feature from lat and lon
create_point_sf <- function(lat, lon, name) {
  point <- sf$st_point(
    x = c(lon, lat),
    dim = "XY"
  ) |>
    sf$st_sfc(
      crs = 4326
    )

  sf$st_sf(
    name = name,
    geometry = point
  )
}

#' Downloading outside the function so just done once
#' Not using {rnaturalearth} because it doesn't allow passing `quiet = TRUE`
#' to `download.file()`
pop_sf <- download_shapefile(
  url = "https://naciscdn.org/naturalearth/110m/cultural/ne_110m_populated_places_simple.zip",
  layer = "ne_110m_populated_places_simple"
)

################
#### UPDATE ####
################

purrr$walk(
  .x = all_iso3_codes(),
  .f = update_cities_sf
)
