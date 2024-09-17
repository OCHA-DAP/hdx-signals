#' Update JRC ASAP code mapping to GAUL and ISO3 codes
box::use(
  dplyr,
  countrycode,
  logger
)

box::use(
  cs = src/utils/cloud_storage
)


# update all the iso3 codes (and names and regions) for locations we want to potentially cover in HDX Signals
df_asap_gaul <- cs$read_az_file("input/gaul1_asap_FAO_Mapping_v8.csv")

########################
#### GET ISO3 CODES ####
########################

df_asap_iso3 <- df_asap_gaul |>
  dplyr$distinct(
    asap0_id,
    adm0_name,
    adm0_code
  ) |>
  dplyr$mutate(
    iso3 = suppressWarnings(
      countrycode$countrycode(
        sourcevar = adm0_code,
        origin = "gaul",
        destination = "iso3c"
      )
    )
  ) |>
  dplyr$filter(
    !is.na(iso3)
  )

fname <- "input/asap_iso3.parquet"
cs$update_az_file(
  df = df_asap_iso3,
  name = fname
)

logger$log_info(paste0("Successfully created ASAP0 to ISO3 mapping and saved to ", fname))
