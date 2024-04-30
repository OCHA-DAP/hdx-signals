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

#' Matches for ISO3 codes lacking UNHCR regions in `countrycode`
region_match_df <- dplyr$tribble(
  ~iso3, ~region_custom,
  "ASM", "Asia and the Pacific",
  "ANT", "The Americas",
  "AZO", "Europe",
  "CAI", "Middle East and North Africa",
  "CHI", "Europe",
  "GLI", "The Americas",
  "IOT", "Asia and the Pacific",
  "CXR", "Asia and the Pacific",
  "CCK", "Asia and the Pacific",
  "FLK", "The Americas",
  "ATF", "Southern Africa",
  "GGY", "Europe",
  "HMD", "Southern Africa",
  "IMN", "Europe",
  "JEY", "Europe",
  "MYT", "Southern Africa",
  "NFK", "Asia and the Pacific",
  "PCN", "Asia and the Pacific",
  "BLM", "The Americas",
  "SHN", "Southern Africa",
  "TWN", "Asia and the Pacific",
  "TKL", "Asia and the Pacific",
  "VIR", "The Americas",
  "UMI", "Asia and the Pacific",
  "WLF", "Asia and the Pacific",
  "ALA", "Europe",
  "XKX", "Europe",
  "AB9", "East and Horn of Africa",
  "LAC", "The Americas"
)

df_names <- df_ocha_names |>
  dplyr$left_join(
    region_match_df,
    by = "iso3"
  ) |>
  dplyr$transmute(
    iso3,
    country,
    region = ifelse(
      is.na(region_custom),
      countrycode$countrycode(
        sourcevar = iso3,
        origin = "iso3c",
        destination = "unhcr.region"
      ),
      region_custom
    )
  ) |>
  dplyr$filter(
    !is.na(region) # drops antarctica
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
