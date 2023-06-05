library(tidyverse)
library(readxl)
library(janitor)
library(countrycode)
library(zoo)

source(
  file.path(
    "src",
    "utils",
    "googledrive.R"
  )
)

source(
  file.path(
    "src",
    "utils",
    "get_country_names.R"
  )
)

# alert helper function
# only generate alert when boundary crossed from below
lim_alert <- function(x, lim) {
  x >= lim & lag(x) < lim
}

##############################
#### DRIVE AND LOCAL DATA ####
##############################

# authorize and prep

# get drive files
drive_raw_cholera <- get_drive_file("raw_cholera")
drive_wrangled_cholera <- get_drive_file("wrangled_cholera")
drive_flags_cholera <- get_drive_file("flags_cholera")

# temp paths for local saving
local_raw_cholera <- tempfile(fileext = ".csv")
local_wrangled_cholera <- tempfile(fileext = ".csv")
local_flags_cholera <- tempfile(fileext = ".csv")

#################
#### CHOLERA ####
#################

# raw data
df_cholera_raw <- read_csv(
  url(
    Sys.getenv("CERF_CHOLERA_DATA")
  )
)

# wrangled data
df_cholera_wrangled <- df_cholera_raw %>%
  clean_names() %>%
  filter(
    event == "Cholera"
  ) %>%
  transmute(
    iso3 = countryname(country, destination = "iso3c"),
    start_date = lubridate::dmy(
      case_when(
        !is.na(start_of_reporting_period) ~ start_of_reporting_period,
        !is.na(start_of_reporting_period_2) ~ start_of_reporting_period_2,
        !is.na(start_of_reporting_period_3) ~ start_of_reporting_period_3
      )
    ),
    date = week_date,
    cholera_cases = total_cases
  ) %>%
  group_by(iso3) %>%
  arrange(
    start_date,
    date,
    .by_group = TRUE
  ) %>%
  mutate(
    cholera_cases = na.approx(cholera_cases)
  ) %>%
  ungroup()

# flagged data

df_cholera_flags <- df_cholera_wrangled %>%
  group_by(iso3) %>%
  mutate(
    flag_1k = lim_alert(cholera_cases, 1000),
    flag_5k = lim_alert(cholera_cases, 5000),
    flag_10k = lim_alert(cholera_cases, 10000),
    flag_stop = cholera_cases < 1000 & lag(cholera_cases, default = 0) >= 1000,
    group = cumsum(flag_stop)
  ) %>%
  ungroup() %>%
  pivot_longer(
    flag_1k:flag_10k,
    names_to = "flag"
  ) %>%
  filter(
    value
  ) %>%
  group_by(
    iso3,
    start_date,
    group
  ) %>%
  summarize(
    end_date = max(date),
    latest_flag = tail(flag, n = 1),
    message = paste(
      "There have been",
      scales::comma(tail(cholera_cases, n = 1)),
      "cholera cases reported since",
      gsub("^0", "", format(min(start_date), format = "%d %B %Y."))
    ),
    .groups = "drop"
  ) %>%
  get_country_names() %>%
  mutate(
    flag_type = "cholera",
    flag_source = "who",
    .before = start_date
  ) %>%
  select(
    -group
  )

############################################
#### COMPARE WITH PREVIOUS FLAGGED DATA ####
############################################

drive_download(
  file = drive_flags_cholera,
  path = local_flags_cholera
)

df_cholera_flags_prev <- read_csv(
  local_flags_cholera
)

df_cholera_flags_new <- anti_join(
  df_cholera_flags,
  df_cholera_flags_prev,
  by = c("iso3", "start_date", "end_date")
) %>%
  mutate(
    email = TRUE
  )

df_cholera_flags_final <- semi_join(
  df_cholera_flags,
  df_cholera_flags_prev,
  by = c("iso3", "start_date", "end_date")
) %>%
  mutate(
    email = FALSE
  ) %>%
  bind_rows(
    df_cholera_flags_new
  ) %>%
  mutate(
    url = NA,
    .before = email
  ) %>%
  mutate(
    summary_experimental = NA,
    .after = email
  )

########################
#### SAVE IPC  DATA ####
########################

update_drive_file(
  df = df_cholera_raw,
  local_path = local_raw_cholera,
  drive_file = drive_raw_cholera
)

update_drive_file(
  df = df_cholera_wrangled,
  local_path = local_wrangled_cholera,
  drive_file = drive_wrangled_cholera
)

update_drive_file(
  df = df_cholera_flags_final,
  local_path = local_flags_cholera,
  drive_file = drive_flags_cholera
)
