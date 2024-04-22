box::use(dplyr)
box::use(countrycode)
box::use(httr2)
box::use(purrr)
box::use(sf)
box::use(ripc)
box::use(tidyr)

# prevent geometry errors
sf$sf_use_s2(FALSE)

####################
#### ISO3 CODES ####
####################

iso3_codes <- countrycode$codelist |>
  dplyr$filter(
    !is.na(iso3c)
  ) |>
  dplyr$pull(
    iso3c
  )

# add in some additional codes not standard
iso3_codes <- c(iso3_codes, "XKX", "AB9", "LAC")

#######################
#### GET CENTROIDS ####
#######################

un_geodata_sf <- cs$read_az_file("input/un_geodata.geojson")

# get centroid coordinates from the UN Geodata geojson file
sf_centroids <- un_geodata_sf |>
  dplyr$group_by(
    iso3 = iso3cd
  ) |>
  dplyr$summarize(
    geometry = sf$st_union(geometry)
  ) |>
  sf$st_centroid() |>
  dplyr$mutate(
    centroid_coords = as.data.frame(sf$st_coordinates(geometry))
  ) |>
  tidyr$unnest(centroid_coords) |>
  dplyr$transmute(
    iso3,
    lat = Y,
    lon = X
  ) |>
  sf$st_drop_geometry()

#' Get centroid from Geoboundaries data
get_geoboundaries_centroid <- function(iso3) {
  httr2$request(
    "https://www.geoboundaries.org/api/current/gbOpen/XKX/ADM0"
  ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("simplifiedGeometryGeoJSON") |>
    sf$read_sf() |>
    sf$st_centroid() |>
    sf$st_coordinates() |>
    dplyr$tibble() |>
    dplyr$mutate(iso3 = !!iso3)
}

#' Custom centroids for a few countries or regions that we know we need centroids
#' for, not in the original data file
custom_centroid <- function(iso3) {
  # Abyei Area just use this dataset found online
  if (iso3 == "AB9") {
    temp <- tempfile()
    temp_uz <- tempfile()
    download.file(
      url = "https://open.africa/dataset/56d1d233-0298-4b6a-8397-d0233a1509aa/resource/76c698c9-e282-4d00-9202-42bcd908535b/download/ssd_admbnda_abyei_imwg_nbs_20180401.zip",
      destfile = temp
    )
    unzip(zipfile = temp, exdir = temp_uz)
    sf$read_sf(temp_uz) |>
      sf$st_centroid() |>
      sf$st_coordinates() |>
      dplyr$as_tibble() |>
      dplyr$mutate(
        iso3 = "AB9"
      )
  } else if (iso3 == "LAC") {
    # for Latin America and Caribbean, use centroid from IPC
    ripc$ipc_get_areas(
      country = "LAC",
      return_format = "geojson"
    ) |>
      dplyr$slice(1) |>
      sf$st_centroid() |>
      sf$st_coordinates() |>
      dplyr$as_tibble() |>
      dplyr$mutate(
        iso3 = "LAC"
      )
  } else if (iso3 == "XKX") {
    get_geoboundaries_centroid("XKX")
  } else {
    data.frame(
      X = NA_real_,
      Y = NA_real_,
      iso3 = iso3
    )
  }
}

df_centroids <- purrr$map(
  .x = iso3_codes,
  .f = get_centroid
) |>
  purrr$list_rbind() |>
  dplyr$select(
    iso3,
    lon = X,
    lat = Y
  )

#######################
#### COUNTRY NAMES ####
#######################

#' Matches for ISO3 codes not in `countrycode`
name_match_df <- data.frame(
  iso3 = c("XKX", "AB9", "LAC"),
  `un.name.en` = c("Kosovo", "Abyei Area", "Latin America and the Caribbean"),
  `unhcr.region` = c("Europe", "East and Horn of Africa", "The Americas")
)

#' Get country info for ISO3 codes
#'
#' Gets country info for ISO3 codes. Pulls specified `destination` value from
#' the `match_df` data frame or `countrycode`.
#'
#' Ensures that no warnings are generated from missing ISO3 codes in `countrycode`.
#'
#' @param iso3 Vector of ISO3 codes
#' @param destination Destination variable, either country name or region
get_country_names <- function(iso3, destination) {
  x <- character(length(iso3))
  iso3_custom <- iso3 %in% name_match_df$iso3
  x[iso3_custom] <- name_match_df[[destination]][match(iso3[iso3_custom], name_match_df$iso3)]
  x[!iso3_custom] <- countrycode$countrycode(
    sourcevar = iso3[!iso3_custom],
    origin = "iso3c",
    destination = destination
  )

  if (any(is.na(x))) {
    stop(
      "Missing value for '",
      destination,
      "', check that ISO3 code is correct and accounted for when creating the ",
      "alerts data frame."
    )
  }

  x
}

df_names <- dplyr$tibble(
  iso3 = iso3_codes,
  country = get_country_names(iso3, "un.name.en"),
  region = get_country_names(iso3, "unhcr.region")
)

#######################
#### HRP COUNTRIES ####
#######################

df_hrp <- dplyr$tibble(
  iso3 = iso3_codes,
  hrp_country = iso3 %in% cs$read_az_file("input/hrp_countries.parquet")$iso3
)

#######################
#### FINAL DATASET ####
#######################

df_country_info <- purrr$reduce(
  .x = list(df_names, df_hrp, df_centroids),
  .f = \(x, y) dplyr$left_join(x, y, by = "iso3")
)

# Temporarily remove as test run if set
orig <- Sys.getenv("GMAS_TEST_RUN")
Sys.setenv(GMAS_TEST_RUN = FALSE)

cs$update_az_file(
  df = df_country_info,
  name = "input/country_info.parquet"
)

# Reset env var
if (orig == "") {
  Sys.unsetenv("GMAS_TEST_RUN")
} else {
  Sys.setenv(GMAS_TEST_RUN = orig)
}
