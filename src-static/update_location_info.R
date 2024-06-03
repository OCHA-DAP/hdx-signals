box::use(dplyr)
box::use(countrycode)
box::use(purrr)
box::use(sf)
box::use(ripc)
box::use(idmc)
box::use(readr)
box::use(stringr)
box::use(logger[log_info])

box::use(../src/utils/get_iso3_sf)
box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/hs_logger)

hs_logger$configure_logger()

log_info("Updating location info...")

# prevent geometry errors
sf$sf_use_s2(FALSE)

############################
#### GET BASE LOCATIONS ####
############################

# we use the base locations defined in `update_locations.R`
# and generate information for all of those locations
df_locations <- cs$read_az_file("input/locations.parquet")
iso3_codes <- df_locations$iso3

#######################
#### GET CENTROIDS ####
#######################

df_centroids <- purrr$map(
  .x = iso3_codes,
  .f = \(iso3) {
    get_iso3_sf$get_iso3_sf(iso3 = iso3, file = "centroids") |>
      sf$st_coordinates() |>
      dplyr$as_tibble() |>
      dplyr$mutate(
        iso3 = iso3
      )
  },
  .progress = TRUE
) |>
  purrr$list_rbind() |>
  dplyr$transmute(
    iso3,
    lat = Y,
    lon = X
  )

#######################
#### HRP LOCATIONS ####
#######################

df_hrp <- dplyr$tibble(
  iso3 = iso3_codes,
  hrp_location = iso3 %in% cs$read_az_file("input/hrp_locations.parquet")$iso3
)

############################
#### INDICATOR COVERAGE ####
############################

# automatically read in coverage from any coverage files in Azure
coverage_files <- cs$az_file_detect(".*/coverage.parquet")

df_coverage <- purrr$map(
  .x = coverage_files,
  .f = \(fp) {
    indicator_id <- stringr$str_extract(fp, "(?<=output/)(.*)(?=/coverage.parquet)")
    dplyr$tibble(
      "{indicator_id}" := iso3_codes %in% cs$read_az_file(fp)$iso3
    )
  }
) |>
  purrr$list_cbind() |>
  dplyr$mutate(
    iso3 = iso3_codes,
    .before = 1
  )

#######################
#### FINAL DATASET ####
#######################

df_location_info <- purrr$reduce(
  .x = list(df_names, df_hrp, df_centroids, df_coverage),
  .f = \(x, y) dplyr$left_join(x, y, by = "iso3")
)

fname <- "input/location_info.parquet"
cs$update_az_file(
  df = df_location_info,
  name = fname
)

# TEMP: switch when system can read in parquet from Azure prod
cs$update_az_file(
  df = dplyr$select(df_location_info, -lat, -lon),
  name = "signals_location_metadata.csv",
  stage = "dev"
)

log_info(paste0("Successfully downloaded locations info and saved to ", fname))
