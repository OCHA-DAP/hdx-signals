box::use(dplyr)
box::use(countrycode)
box::use(httr2)
box::use(purrr)
box::use(sf)
box::use(ripc)
box::use(tidyr)

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
  .f = \(iso3) get_iso3_sf$get_iso3_sf(iso3 = iso3, file = "centroids"),
  .progress = TRUE
) |>
  purrr$list_rbind() |>
  dplyr$select(-geometry)

#######################
#### COUNTRY NAMES ####
#######################

#' Matches for ISO3 codes not in `countrycode`
name_match_df <- dplyr$tribble(
  ~iso3, ~name_custom, ~region_custom,
  "ASM", "American Samoa", "Asia and the Pacific",
  "IOT", "British Indian Ocean Territory", "Asia and the Pacific",
  "CXR", "Christmas Island", "Asia and the Pacific",
  "CCK", "Cocos (Keeling) Islands", "Asia and the Pacific",
  "FLK", "Falkland Islands", "The Americas",
  "ATF", "French Southern Territories", "Southern Africa",
  "GGY", "Guernsey", "Europe",
  "HMD", "Heard Island and McDonald Islands", "Southern Africa",
  "IMN", "Isle of Man", "Europe",
  "JEY", "Jersey", "Europe",
  "MYT", "Mayotte", "Southern Africa",
  "NFK", "Norfolk Island", "Asia and the Pacific",
  "PCN", "Pitcairn Islands", "Asia and the Pacific",
  "BLM", "Saint Barthélemy", "The Americas",
  "SHN", "Saint Helena", "Southern Africa",
  "TWN", "China - Taiwan Province", "Asia and the Pacific",
  "TKL", "Tokelau", "Asia and the Pacific",
  "VIR", "U.S. Virgin Islands", "The Americas",
  "UMI", "U.S. Minor Outlying Islands", "Asia and the Pacific",
  "WLF", "Wallis and Futuna", "Asia and the Pacific",
  "ALA", "Åland Islands", "Europe",
  "XKX", "Kosovo", "Europe",
  "AB9", "Abyei Area", "East and Horn of Africa",
  "LAC", "Tri-national Border of Rio Lempa", "The Americas"
)

df_names <- dplyr$tibble(
  iso3 = iso3_codes
) |>
  dplyr$left_join(
    countrycode$codelist,
    by = c("iso3" = "iso3c")
  ) |>
  dplyr$left_join(
    name_match_df,
    by = "iso3"
  ) |>
  dplyr$transmute(
    iso3 = iso3,
    country = dplyr$case_when(
      !is.na(name_custom) ~ name_custom,
      !is.na(un.name.en) ~ un.name.en,
      TRUE ~ country.name.en
    ),
    region = ifelse(
      is.na(unhcr.region),
      region_custom,
      unhcr.region
    )
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
