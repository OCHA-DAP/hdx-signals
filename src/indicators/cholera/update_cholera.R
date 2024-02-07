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
box::use(purrr)

# internal utilities
box::use(cs = ../../utils/cloud_storage)
box::use(../../utils/format_date[format_date])
box::use(./plot_cholera)

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
      stringr$str_detect(start_date_raw, "[0-9]{1,2}[-|//][A-Za-z]{3}") ~ lubridate$dmy(start_date_raw),
      stringr$str_detect(start_date_raw, "^[A-Za-z]{3}") ~ lubridate$mdy(start_date_raw),
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
    `Medium concern` = lim_alert(cholera_cases, 1000),
    `High concern` = lim_alert(cholera_cases, 5000),
  ) |>
  dplyr$ungroup() |>
  tidyr$pivot_longer(
    dplyr$ends_with("concern"),
    names_to = "alert_level"
  ) |>
  dplyr$filter(
    value
  ) |>
  dplyr$group_by(
    iso3,
    start_date
  ) |>
  dplyr$summarize(
    date = max(date),
    alert_level = utils$tail(alert_level, n = 1),
    value = tail(cholera_cases, n = 1),
    message = paste0(
      scales$comma_format()(tail(cholera_cases, n = 1)),
      " cholera cases reported since ",
      format_date(min(start_date))
    ),
    .groups = "drop"
  ) |>
  dplyr$mutate(
    country = countrycode$countrycode(iso3, "iso3c", "cldr.short.en"),
    indicator_name = "cholera",
    indicator_source = "who",
    .after = iso3
  ) |>
  dplyr$select(
    -start_date
  )

df_cholera_flags_new <- df_cholera_flags |>
  dplyr$mutate(
    plot = purrr$pmap_chr(
      .l = list(
        iso3 = iso3,
        title = message,
        date = date
      ),
      .f = \(iso3, title, date) plot_cholera$plot_timeline(iso3, title, date, df_cholera_wrangled, TRUE)
    ),
    map = NA,
    source_url = "https://www.afro.who.int/health-topics/disease-outbreaks/outbreaks-and-other-emergencies-updates",
    other_urls = NA,
    further_information = NA
  )

############################################
#### COMPARE WITH PREVIOUS FLAGGED DATA ####
############################################

df_cholera_flags_prev <- cs$read_gcs_file(
  name = "output/cholera/flags.parquet"
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

df_cholera_flags_new |> dplyr$mutate(summary = NA, hdx_url = NA, .after = map) |> cs$update_gcs_file("output/cholera/flags.parquet")

########################
#### SAVE IPC  DATA ####
########################

cs$update_gcs_file(
  df = df_cholera_raw,
  name = "output/cholera/raw.parquet"
)

cs$update_gcs_file(
  df = df_cholera_wrangled,
  name = "output/cholera/wrangled.parquet"
)

cs$update_gcs_file(
  df = df_cholera_flags_final,
  name = "output/cholera/flags.parquet"
)
