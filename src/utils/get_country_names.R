library(tidyverse)
library(countrycode)
source(
  file.path(
    "src",
    "utils",
    "googledrive.R"
  )
)

#################################
#### READ COUNTRY NAMES DATA ####
#################################

drive_country_names <- get_drive_file("cerf_dashboard_names")
drive_download(drive_country_names, f <- tempfile(fileext = ".csv"))
df_names <- read_csv(f)

########################
#### UTIL FUNCTIONS ####
########################

# simple function just adds country names after the iso3 column
# pulled from this file
get_country_names <- function(df) {
  if ("country" %in% names(df)) {
    df <- select(df, -country)
  }

  left_join(df, df_names, by = "iso3") %>%
    relocate(country, .after = iso3) %>%
    mutate(
      country = ifelse(
        is.na(country),
        countrycode(iso3, origin = "iso3c", destination = "country.name.en"),
        country
      )
    )
}
