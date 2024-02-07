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

alert <- function(df_wrangled, recreate = FALSE) {
  df_alerts <- base_alert(df_wrangled)
  df_alerts_prev <- cs$read_gcs_file(
    name = "output/cholera/flags.parquet"
  )

  if (!recreate) {
    df_alerts_new <- new_alerts(df_alerts, df_alerts_prev)
    df_campaign_new <- campaign_info(df_alerts_new, df_wrangled, recreate)
  } else {
    campaign_info(df_alerts, df_wrangled, recreate)
  }
}

#' Creates base alert dataset
#'
#' Creates base alert dataset for cholera. Output will be compared with the
#' existing alerts dataset to identify where new alerts have been found and
#' generate a campaign as necessary. In this case, alerts are generated when
#' cases cross 1,000 or 5,000 from below.
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
base_alert <- function(df_wrangled) {
  df_wrangled |>
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
}

#' Alert when limit crossed from below
#'
#' @param x Vector
#' @param lim Limit
#'
#' @returns Boolean vector
lim_alert <- function(x, lim) {
  x >= lim & dplyr$lag(x) < lim
}

#' Find new alerts
#'
#' Compares the alerts dataset with the previous to find new alerts.
#'
#' @param df_alerts Current alerts
#' @param df_alerts_prev Previous alerts
#'
#' @returns Data frame of new alerts, those found in `df_alerts` but not in
#' `df_alerts_prev`
new_alerts <- function(df_alerts, df_alerts_prev) {
  dplyr$anti_join(
    df_alerts,
    df_alerts_prev,
    by = c("iso3", "alert_level", "date")
  ) |>
    dplyr$mutate(
      email = TRUE
    )
}

#' Add campaign info to alerts
#'
#' Adds campaign information to alerts dataset, including plots, maps, relevant
#' links and other information to include in the campaign. If `recreate`, plots
#' are recreated with filtering down to the date. Otherwise, plots are created
#' across the full length of the data.
#'
#' @param df_alerts Alerts dataset to create campaign info for
#' @param df_wrangled Wrangled data frame
#' @param recreate Whether or not we are recreating the campaign information, which
#'     determines the timeline of the plots in `plot_timeline()`. `email` set to
#'     the opposite of `recreate`.
#'
#' @returns Data frame with campaign information
campaign_info <- function(df_alerts, df_wrangled, recreate) {
  df_alerts |>
    dplyr$mutate(
      plot = purrr$pmap_chr(
        .l = list(
          iso3 = iso3,
          title = message,
          date = date
        ),
        .f = \(iso3, title, date) plot_cholera$plot_timeline(
          iso3 = iso3,
          title = title,
          date = date,
          df = df_wrangled,
          date_filter = recreate
        )
      ),
      map = NA_character_,
      summary = NA_character_,
      hdx_url = NA_character_,
      source_url = "https://www.afro.who.int/health-topics/disease-outbreaks/outbreaks-and-other-emergencies-updates",
      other_urls = NA_character_,
      further_information = NA_character_
    )
}
