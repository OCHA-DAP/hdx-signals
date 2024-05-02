box::use(dplyr)
box::use(countrycode)
box::use(readxl)
box::use(stringr)

box::use(cs = ../src/utils/cloud_storage)

# Download 2024 file from HDX
# Needs manual munging since no programmatically readable list of HRP countries
# Will have to update when list changes

# Read file

tf <- tempfile(fileext = ".xlsx")

download.file(
  url = "https://data.humdata.org/dataset/6cb35657-975e-46a0-99a7-a558eddb924f/resource/28be64d3-adaf-4f61-887c-87a8b5d9c625/download/section3_plan_tables_2024-n.xlsx",
  destfile = tf
)

# Temporarily remove as test run if set
orig <- Sys.getenv("GMAS_TEST_RUN")
Sys.setenv(GMAS_TEST_RUN = FALSE)

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
    name = "input/hrp_countries.parquet"
  )

# Reset env var
if (orig == "") {
  Sys.unsetenv("GMAS_TEST_RUN")
} else {
  Sys.setenv(GMAS_TEST_RUN = orig)
}
