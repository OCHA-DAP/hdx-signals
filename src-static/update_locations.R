box::use(
  readr,
  dplyr,
  countrycode,
  logger
)

box::use(
  cs = src/utils/cloud_storage,
  src/utils/hs_logger
)

logger$log_info("Updating locations we cover...")

# update all the iso3 codes (and names and regions) for locations we want to potentially cover in HDX Signals
df_taxonomy <- readr$read_csv(
  file = "https://docs.google.com/spreadsheets/d/1NjSI2LaS3SqbgYc0HdD8oIb7lofGtiHgoKKATCpwVdY/export?format=csv&gid=1088874596", #nolint
  col_types = readr$cols()
)

###########################
#### GET ISO3 AND NAME ####
###########################

df_ocha_names <- df_taxonomy |>
  dplyr$slice(-1) |>
  readr$type_convert(
    col_types = readr$cols()
  ) |>
  dplyr$transmute(
    iso3 = dplyr$case_when(
      `x Alpha3 codes` == "XKX" ~ "XKX",
      .default = `ISO 3166-1 Alpha 3-Codes`
    ),
    location = `Preferred Term`,
    location_note = ifelse(
      is.na(`ISO 3166-1 Alpha 3-Codes`),
      "Custom Alpha 3 code",
      NA_character_
    )
  ) |>
  dplyr$filter(
    !is.na(iso3), # other custom alpha 3 we don't cover like Azores or Galapagos
    iso3 != "ATA" # antarctica
  ) |>
  dplyr$add_row(
    iso3 = c("AB9", "LAC"),
    location = c("Abyei Area", "Tri-national Border of Rio Lempa"),
    location_note = c("Abyei Area reported on in IDMC using AB9 code", "Custom code used by IPC for 3 country analysis")
  )

#####################
#### ADD REGIONS ####
#####################

#' Adding OCHA regions for ISO3 codes with no UNHCR region defined in `countrycode`
#' Because we extract OCHA regions from the UNHCR regions with only minimal
#' conversion necessary.
region_match_df <- dplyr$tribble(
  ~iso3, ~region_custom,
  "ASM", "Asia and the Pacific", # small locations lacking UNHCR regions
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
  "CRQ", "Europe",
  "AB9", "East and Horn of Africa", # custom ISO3 for IDMC
  "LAC", "Latin America and the Caribbean", # custom ISO3 for IPC
  "COD", "West and Central Africa", # UNHCR region doesn't match OCHA region
  "COG", "West and Central Africa", # UNHCR region doesn't match OCHA region
  "USA", "North America", # Creating custom North America region
  "CAN", "North America" # Creating custom North America region
)

df_regions <- df_ocha_names |>
  dplyr$left_join(
    region_match_df,
    by = "iso3"
  )

# don't use dplyr to avoid warnings for missing values in countrycode
uses_custom_reg <- !is.na(df_regions$region_custom)
df_regions$unhcr_region <- NA_character_
df_regions$unhcr_region[uses_custom_reg] <- df_regions$region_custom[uses_custom_reg]
df_regions$unhcr_region[!uses_custom_reg] <- countrycode$countrycode(
  sourcevar = df_regions$iso3[!uses_custom_reg],
  origin = "iso3c",
  destination = "unhcr.region"
)

# pull together unhcr region and custom region into one final dataset
df_locations <- df_regions |>
  dplyr$transmute(
    iso3,
    location,
    location_note,
    region = dplyr$case_when(
      !is.na(region_custom) ~ region_custom,
      unhcr_region == "The Americas" ~ "Latin America and the Caribbean",
      unhcr_region %in% c("Southern Africa", "East and Horn of Africa") ~ "Southern and Eastern Africa",
      .default = unhcr_region
    )
  )

if (any(is.na(dplyr$select(df_locations, -location_note)))) {
  stop(
    "Location date has missing values, fix before re-running.",
    call. = FALSE
  )
} else {
  fname <- "input/locations.parquet"
  cs$update_az_file(
    df = df_locations,
    name = fname
  )
  logger$log_info(paste0("Successfully created locations date and saved to ", fname))
}
