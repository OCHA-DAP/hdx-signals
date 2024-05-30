#' Script to get ACLED metadata on when location time series are complete. The
#' information is stored on a PDF URL.
box::use(dplyr)
box::use(readxl)
box::use(stringr)
box::use(glue)
box::use(logger[log_info])

box::use(../src/utils/location_codes)
box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/hs_logger)

hs_logger$configure_logger()

# TODO: Remove once this passes from GH runner
if (interactive()) {
  log_info("Updating ACLED info...")

  # read excel file of acled location codes from their website
  download.file(
    url = "https://acleddata.com/download/3987/",
    destfile = tf <- tempfile(fileext = ".xlsx"),
    quiet = TRUE
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
      iso3 = location_codes$ison_to_iso3(`ISO Codes`),
      start_date = as.Date(
        x = paste(1, `Start Date`),
        format = "%d %B %Y"
      ),
      acled_hdx_location = Country |>
        stringr$str_remove_all("[^[:alnum:]\\s-\\.]") |>
        stringr$str_remove_all("\\.$") |>
        tolower() |>
        stringr$str_replace_all("\\s|\\.", "-"),
      acled_hdx_url = as.character(
        glue$glue(
          "https://data.humdata.org/dataset/{acled_hdx_location}-acled-conflict-data"
        )
      )
    )

  fname <- "input/acled_info.parquet"
  cs$update_az_file(
    df = df_acled_info,
    name = fname
  )

  log_info(paste0("Successfully downloaded ACLED info and saved to ", fname))

} else {
  log_info("Skipping ACLED data download")
}
