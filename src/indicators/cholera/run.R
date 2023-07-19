library(tidyverse)
library(readxl)
library(janitor)
library(countrycode)
library(zoo)

source(
  file.path(
    "src",
    "utils",
    "google_sheets.R"
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

#################
#### CHOLERA ####
#################

f <- tempfile(fileext = ".csv")
download.file(
  url = Sys.getenv("CERF_CHOLERA_DATA"),
  destfile = f
)

# raw data
df_cholera_raw <- read_csv(
  f,
  col_types = cols(.default = "c")
)

# wrangled data
df_cholera_wrangled <- df_cholera_raw %>%
  clean_names() %>%
  filter(
    str_detect(tolower(event), "cholera")
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
    date = as.Date(week_date),
    cholera_cases = parse_number(total_cases)
  ) %>%
  group_by( # some countries have multiple sets of cases reported each date (DRC)
    iso3,
    start_date,
    date
  ) %>%
  summarize(
    cholera_cases = sum(cholera_cases),
    .groups = "drop"
  ) %>%
  group_by(
    iso3
  ) %>%
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
    flag_2k = lim_alert(cholera_cases, 2000),
    flag_5k = lim_alert(cholera_cases, 5000),
    flag_10k = lim_alert(cholera_cases, 10000)
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
    start_date
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
  )

############################################
#### COMPARE WITH PREVIOUS FLAGGED DATA ####
############################################

df_cholera_flags_prev <- read_gs_file(
  name = "flags_cholera$"
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

update_gs_file(
  df = df_cholera_raw,
  name = "raw_cholera$"
)

update_gs_file(
  df = df_cholera_wrangled,
  name = "wrangled_cholera$"
)

update_gs_file(
  df = df_cholera_flags_final,
  name = "flags_cholera$"
)
