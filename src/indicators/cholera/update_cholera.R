# external packages
box::use(dplyr)
box::use(readr)
box::use(lubridate)
box::use(janitor)
box::use(stringr)
box::use(countrycode)
box::use(zoo)
box::use(tidyr)
box::use(utils)
box::use(scales)

# internal utilities
# first set the root search path for utilities
box::use(gs = ../../utils/google_sheets)
box::use(../../utils/get_country_names[get_country_names])
box::use(../../utils/format_date[format_date])

# alert helper function
# only generate alert when boundary crossed from below
lim_alert <- function(x, lim) {
  x >= lim & dplyr$lag(x) < lim
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
df_cholera_raw <- readr$read_csv(
  f,
  col_types = readr$cols(.default = "c")
)

# wrangled data
df_cholera_wrangled <- df_cholera_raw |>
  janitor$clean_names() |>
  dplyr$filter(
    stringr$str_detect(tolower(event), "cholera") & !stringr$str_detect(tolower(event), "intestinal|bacterial|shigellosis|salmonellosis")
  ) |>
  dplyr$transmute(
    iso3 = countrycode$countryname(country, destination = "iso3c"),
    event,
    start_date_raw = dplyr$case_when(
      !is.na(start_of_reporting_period) ~ start_of_reporting_period,
      !is.na(start_of_reporting_period_2) ~ start_of_reporting_period_2,
      !is.na(start_of_reporting_period_3) ~ start_of_reporting_period_3
    ),
    date = as.Date(week_date),
    start_date = dplyr$case_when( # formats for start dates have switched in bulletins
      stringr$str_detect(start_date_raw, "[A-Za-z]{3}") ~ lubridate$dmy(start_date_raw),
      date >= "2023-09-25" ~ lubridate$mdy(start_date_raw),
      date < "2023-09-25" ~ lubridate$dmy(start_date_raw)
    ),
    cholera_cases = readr$parse_number(total_cases)
  ) |>
  dplyr$select(
    -start_date_raw
  ) |>
  dplyr$group_by( # some countries have multiple sets of cases reported each date (DRC)
    iso3,
    date
  ) |>
  dplyr$summarize(
    event = paste(unique(event), collapse = "; "),
    start_date = min(start_date),
    cholera_cases = sum(cholera_cases),
    .groups = "drop"
  ) |>
  dplyr$group_by(
    iso3
  ) |>
  dplyr$arrange(
    start_date,
    date,
    .by_group = TRUE
  ) |>
  dplyr$mutate(
    cholera_cases = zoo$na.approx(cholera_cases)
  ) |>
  dplyr$ungroup()

# flagged data

df_cholera_flags <- df_cholera_wrangled |>
  dplyr$group_by(iso3) |>
  dplyr$mutate(
    flag_1k = lim_alert(cholera_cases, 1000),
    flag_2k = lim_alert(cholera_cases, 2000),
    flag_5k = lim_alert(cholera_cases, 5000),
    flag_10k = lim_alert(cholera_cases, 10000)
  ) |>
  dplyr$ungroup() |>
  tidyr$pivot_longer(
    flag_1k:flag_10k,
    names_to = "flag"
  ) |>
  dplyr$filter(
    value
  ) |>
  dplyr$group_by(
    iso3,
    start_date
  ) |>
  dplyr$summarize(
    end_date = max(date),
    latest_flag = utils$tail(flag, n = 1),
    message = paste0(
      "<b>WHO reported event:</b> ",
      unique(event),
      "\n\n",
      "There have been ",
      scales$comma_format()(tail(cholera_cases, n = 1)),
      " cases reported since ",
      format_date(min(start_date)),
      "."
    ),
    .groups = "drop"
  ) |>
  get_country_names() |>
  dplyr$mutate(
    flag_type = "cholera",
    flag_source = "who",
    .before = start_date
  )

############################################
#### COMPARE WITH PREVIOUS FLAGGED DATA ####
############################################

df_cholera_flags_prev <- gs$read_gs_file(
  name = "flags_cholera"
)

df_cholera_flags_new <- dplyr$anti_join(
  df_cholera_flags,
  df_cholera_flags_prev,
  by = c("iso3", "start_date", "end_date")
) |>
  dplyr$mutate(
    email = TRUE
  )

df_cholera_flags_final <- dplyr$semi_join(
  df_cholera_flags,
  df_cholera_flags_prev,
  by = c("iso3", "start_date", "end_date")
) |>
  dplyr$mutate(
    email = FALSE
  ) |>
  dplyr$bind_rows(
    df_cholera_flags_new
  ) |>
  dplyr$mutate(
    url = NA,
    .before = email
  ) |>
  dplyr$mutate(
    summary_experimental = NA,
    .after = email
  )

########################
#### SAVE IPC  DATA ####
########################

gs$update_gs_file(
  df = df_cholera_raw,
  name = "raw_cholera"
)

gs$update_gs_file(
  df = df_cholera_wrangled,
  name = "wrangled_cholera"
)

gs$update_gs_file(
  df = df_cholera_flags_final,
  name = "flags_cholera"
)
