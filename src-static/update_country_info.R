box::use(dplyr)
box::use(countrycode)
box::use(httr2)
box::use(purrr)
box::use(sf)
box::use(ripc)
box::use(tidyr)
box::use(readr)

box::use(../src/utils/get_iso3_sf)
box::use(../src/utils/all_iso3_codes)
box::use(cs = ../src/utils/cloud_storage)

# prevent geometry errors
sf$sf_use_s2(FALSE)

####################
#### ISO3 CODES ####
####################

iso3_codes <- all_iso3_codes$all_iso3_codes()

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
#### COUNTRY NAMES ####
#######################

df_ocha_names <- readr$read_csv("https://docs.google.com/spreadsheets/d/1NjSI2LaS3SqbgYc0HdD8oIb7lofGtiHgoKKATCpwVdY/export?format=csv&gid=1088874596") |>
  dplyr$slice(-1) |>
  readr$type_convert() |>
  dplyr$transmute(
    iso3 = ifelse(
      is.na(`ISO 3166-1 Alpha 3-Codes`),
      `x Alpha3 codes`,
      `ISO 3166-1 Alpha 3-Codes`
    ),
    country = `Preferred Term`
  ) |>
  dplyr$filter(
    !is.na(iso3) # drops Sark
  ) |>
  dplyr$add_row(
    iso3 = c("AB9", "LAC"),
    country = c("Abyei Area", "Tri-national Border of Rio Lempa")
  )

#' Adding OCHA regions for ISO3 codes with no UNHCR region defined in `countrycode`
#' Because we extract OCHA regions from the UNHCR regions with only minimal
#' conversion necessary.
region_match_df <- dplyr$tribble(
  ~iso3, ~region_custom,
  "ASM", "Asia and the Pacific", # small countries lacking UNHCR regions
  "ANT", "Latin America and the Caribbean",
  "AZO", "Europe",
  "CAI", "Middle East and North Africa",
  "CHI", "Europe",
  "GLI", "Latin America and the Caribbean",
  "IOT", "Asia and the Pacific",
  "CXR", "Asia and the Pacific",
  "CCK", "Asia and the Pacific",
  "FLK", "Latin America and the Caribbean",
  "ATF", "Southern and Eastern Africa",
  "GGY", "Europe",
  "HMD", "Southern and Eastern Africa",
  "IMN", "Europe",
  "JEY", "Europe",
  "MYT", "Southern and Eastern Africa",
  "NFK", "Asia and the Pacific",
  "PCN", "Asia and the Pacific",
  "BLM", "Latin America and the Caribbean",
  "SHN", "Southern and Eastern Africa",
  "TWN", "Asia and the Pacific",
  "TKL", "Asia and the Pacific",
  "VIR", "Latin America and the Caribbean",
  "UMI", "Asia and the Pacific",
  "WLF", "Asia and the Pacific",
  "ALA", "Europe",
  "XKX", "Europe",
  "AB9", "East and Horn of Africa", # custom ISO3 for IDMC
  "LAC", "Latin America and the Caribbean", # custom ISO3 for IPC
  "COD", "West and Central Africa", # UNHCR region doesn't match OCHA region
  "COG", "West and Central Africa", # UNHCR region doesn't match OCHA region
  "USA", "North America", # Creating custom North America region
  "CAN", "North America" # Creating custom North America region
)

df_names <- df_ocha_names |>
  dplyr$left_join(
    region_match_df,
    by = "iso3"
  ) |>
  dplyr$transmute(
    iso3,
    country,
    unhcr_region = countrycode$countrycode(
      sourcevar = iso3,
      origin = "iso3c",
      destination = "unhcr.region"
    ),
    region = dplyr$case_when(
      !is.na(region_custom) ~ region_custom,
      unhcr_region == "The Americas" ~ "Latin America and the Caribbean",
      unhcr_region %in% c("Southern Africa", "East and Horn of Africa") ~ "Southern and Eastern Africa",
      .default = unhcr_region
    )
  ) |>
  dplyr$filter(
    iso3 != "ATA" # drops antarctica
  ) |>
  dplyr$select(
    -unhcr_region
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

cs$update_az_file(
  df = df_country_info,
  name = "input/country_info.parquet"
)
