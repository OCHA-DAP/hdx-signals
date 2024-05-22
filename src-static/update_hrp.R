box::use(dplyr)
box::use(countrycode)
box::use(readxl)
box::use(stringr)
box::use(logger[log_info])

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/logger)

logger$configure_logger()

log_info("Updating HRP info...")

# Download 2024 file from HDX
# Needs manual munging since no programmatically readable list of HRP countries
# Will have to update when list changes

# Read file

tf <- tempfile(fileext = ".xlsx")
fname <- "input/hrp_countries.parquet"

download.file(
  url = "https://data.humdata.org/dataset/6cb35657-975e-46a0-99a7-a558eddb924f/resource/28be64d3-adaf-4f61-887c-87a8b5d9c625/download/section3_plan_tables_2024-n.xlsx", # nolint
  destfile = tf,
  quiet = TRUE
)

# Read in specific range, get ISO3, and save out
readxl$read_excel(
  path = tf,
  sheet = "GHO 2024 (Overview)",
  skip = 1
) |>
  dplyr$filter(
    !stringr$str_detect(Plan, "\\*"),
    `Plan type` == "HRP"
  ) |>
  dplyr$transmute(
    iso3 = countrycode$countryname(
      sourcevar = Plan,
      destination = "iso3c"
    )
  ) |>
  cs$update_az_file(
    name = fname
  )

log_info(paste0("Successfully downloaded HRP info and saved to ", fname))
