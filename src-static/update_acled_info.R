#' Script to get ACLED metadata on when country time series are complete. The
#' information is stored on a PDF URL.
box::use(dplyr)
box::use(readxl)
box::use(stringr)
box::use(glue)

box::use(../src/utils/country_codes)
box::use(cs = ../src/utils/cloud_storage)

# read excel file of acled country codes from their website
download.file(
  url = "https://acleddata.com/download/3987/",
  destfile = tf <- tempfile(fileext = ".xlsx")
)

df <- readxl$read_excel(tf)

# turn start dates into dates and get iso3 codes
df_acled_info <- df |>
  dplyr$mutate(
    Country = ifelse(
      Country == "Saint Vincent and Grenadines",
      "Saint Vincent and the Grenadines", # to match HDX
      Country
    )
  ) |>
  dplyr$transmute(
    iso3 = country_codes$ison_to_iso3(`ISO Codes`),
    start_date = as.Date(
      x = paste(1, `Start Date`),
      format = "%d %B %Y"
    ),
    acled_hdx_country = Country |>
      stringr$str_remove_all("[^[:alnum:]\\s-\\.]") |>
      stringr$str_remove_all("\\.$") |>
      tolower() |>
      stringr$str_replace_all("\\s|\\.", "-"),
    acled_hdx_url = as.character(glue$glue("https://data.humdata.org/dataset/{acled_hdx_country}-acled-conflict-data"))
  )

cs$update_az_file(
  df = df_acled_info,
  name = "input/acled_info.parquet"
)