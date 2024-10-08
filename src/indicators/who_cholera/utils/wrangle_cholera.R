box::use(
  dplyr,
  readr,
  lubridate,
  janitor,
  stringr,
  zoo
)

box::use(src/utils/location_codes)

#' Wrangle cholera data
#'
#' Given raw data frame, wrangle the cholera data. It is in a rough format
#' from scraping direct from the WHO PDFs, thus a lot of the wrangling is just
#' error checking and cleaning up columns that have been accidentally split up
#' and separated. the output is a weekly time series of cumulative cases since
#' the start date.
#'
#' @param df_raw Raw cholera data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    janitor$clean_names() |>
    dplyr$filter( # need to drop some mentions of possible cholera and other issues
      stringr$str_detect(tolower(event), "cholera") &
        !stringr$str_detect(tolower(event), "intestinal|bacterial|shigellosis|salmonellosis")
    ) |>
    dplyr$transmute(
      iso3 = location_codes$names_to_iso3(country),
      event,
      start_date_raw = dplyr$case_when(
        !is.na(start_of_reporting_period) ~ start_of_reporting_period,
        !is.na(start_of_reporting_period_2) ~ start_of_reporting_period_2,
        !is.na(start_of_reporting_period_3) ~ start_of_reporting_period_3
      ),
      date = as.Date(week_date),
      start_date = dplyr$case_when( # formats for start dates have switched in bulletins
        stringr$str_detect(start_date_raw, "[0-9]{1,2}[-|//][A-Za-z]{3}") ~ lubridate$dmy(start_date_raw, quiet = TRUE),
        stringr$str_detect(start_date_raw, "^[A-Za-z]{3}") ~ lubridate$mdy(start_date_raw, quiet = TRUE),
        date >= "2023-09-25" ~ lubridate$mdy(start_date_raw, quiet = TRUE),
        date < "2023-09-25" ~ lubridate$dmy(start_date_raw, quiet = TRUE)
      ),
      cholera_cases = readr$parse_number(total_cases)
    ) |>
    dplyr$select(
      -start_date_raw
    ) |>
    dplyr$group_by( # some locations have multiple sets of cases reported each date (DRC)
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
      date,
      .by_group = TRUE
    ) |>
    dplyr$mutate(
      # TODO: data is missing due to scraping errors, CERF should fix
      cholera_cases = zoo$na.approx(cholera_cases, na.rm = FALSE) # infill missing data
    ) |>
    dplyr$ungroup()
}
