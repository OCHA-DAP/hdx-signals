box::use(
  dplyr,
  purrr,
  sf,
  stringr,
  logger,
  glue
)

box::use(
  cs = src/utils/cloud_storage,
  src/utils/hs_logger,
  src/utils/push_hdx
)


logger$log_info("Updating location info...")

# prevent geometry errors
suppressMessages(
  sf$sf_use_s2(FALSE)
)

############################
#### GET BASE LOCATIONS ####
############################

# we use the base locations defined in `update_locations.R`
# and generate information for all of those locations
df_locations <- cs$read_az_file("input/locations.parquet")
iso3_codes <- df_locations$iso3

##################################
#### GET CENTROIDS AND SOURCE ####
##################################

df_centroids <- purrr$map(
  .x = iso3_codes,
  .f = \(iso3) cs$read_az_file(glue$glue("input/centroids/{iso3}.parquet")),
  .progress = interactive()
) |>
  purrr$list_rbind()

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

df_locations_metadata <- purrr$reduce(
  .x = list(df_locations, df_hrp, df_centroids, df_coverage),
  .f = \(x, y) dplyr$left_join(x, y, by = "iso3")
)

fname <- "output/signals_locations_metadata.parquet"
cs$update_az_file(
  df = df_locations_metadata,
  name = fname
)

logger$log_info(paste0("Successfully downloaded locations info and saved to ", fname))

# saving out to CSV at the same time
fname <- "output/signals_locations_metadata.csv"
cs$update_az_file(
  df = df_locations_metadata,
  name = fname
)

logger$log_info(paste0("Successfully downloaded locations info and saved to ", fname))

push_hdx$push_hdx()

logger$log_info(paste0("Successfully trigger webhook to push ", fname, " to HDX"))
