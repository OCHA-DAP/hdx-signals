# external packages
box::use(dplyr)
box::use(countrycode)
box::use(purrr)
box::use(glue)
box::use(tidyr)
box::use(utils)
box::use(scales)

# internal utilities
box::use(./plot_cholera)
box::use(cs = ../../../utils/cloud_storage)
box::use(../../../utils/format_date[format_date])
box::use(../../../email/email)

#' Generate alerts information and create campaign
#'
#' Generates alerts for cholera data. This creates a data frame with all information
#' relevant for creating email campaigns in `email$generate_campaigns()`, including
#' putting plots onto the Mailchimp servers.
#'
#' If `recreate` is `TRUE`, then the alerts dataset is recreated and new campaigns
#' are created
#'
#' @param df_wrangled Data frame of wrangled data
#' @param recreate Whether or not to recreate the alerts dataset
#'
#' @export
alert <- function(df_wrangled, recreate = FALSE) {
  df_alerts <- base_alert(df_wrangled)
  df_alerts_prev <- cs$read_az_file(
    name = "output/cholera/flags.parquet"
  )

  if (!recreate) {
    df_alerts_new <- new_alerts(df_alerts, df_alerts_prev) |>
      campaign_info(df_wrangled, recreate)

    df_alerts_new$campaign <- email$generate_campaigns(
      indicator_id = "who_cholera",
      shock_title = "Cholera",
      template_folder = "HDX Signals - Cholera",
      alerts_df = df_campaign_new
    )

    df_campaign <- dplyr$bind_rows(df_alerts_prev, df_alerts_new)
  } else {
    # create a new campaign data frame
    # with each row having a separate campaign URL since cholera alerts
    # are distinct from each other
    input <- readline(
      paste0(
        "You set `recreate` to `TRUE`. This will overwrite the existing ",
        "alerts dataset and create new campaigns on Mailchimp. Enter Y to confirm:\n\n"
      )
    )
    if (tolower(input) == "y") {
      df_campaign <- campaign_info(df_alerts, df_wrangled, recreate)
      df_campaign$campaign <- purrr$map_chr(
        .x = split(df_campaign, 1:nrow(df_campaign)),
        .f = \(df) {
          email$generate_campaigns(
            indicator_id = "who_cholera",
            shock_title = "Cholera",
            template_folder = "HDX Signals - Cholera",
            alerts_df = df,
            send_email = FALSE
          )
        }
      )
    } else {
      message(
        "No alerts generated as you did not confirm recreation of alerts data."
      )
      return(NULL)
    }
  }

  df_campaign
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
      value = utils$tail(cholera_cases, n = 1),
      message = paste0(
        scales$comma_format()(utils$tail(cholera_cases, n = 1)),
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
#'     determines the timeline of the plots in `plot_timeline()`.
#'
#' @returns Data frame with campaign information
campaign_info <- function(df_alerts, df_wrangled, recreate) {
  # generate plot
  cholera_plot <- purrr$pmap(
    .l = list(
      iso3 = df_alerts$iso3,
      title = df_alerts$message,
      date = df_alerts$date
    ),
    .f = \(iso3, title, date) plot_cholera$plot_timeline(
      iso3 = iso3,
      title = title,
      date = date,
      df = df_wrangled,
      date_filter = recreate
    )
  ) |>
    purrr$list_rbind()

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
      other_images = NA_character_,
      summary = NA_character_,
      hdx_url = NA_character_,
      source_url = "https://www.afro.who.int/health-topics/disease-outbreaks/outbreaks-and-other-emergencies-updates",
      other_urls = NA_character_,
      further_information = glue$glue(
        'Refer to the <a href="{source_url}">WHO Afro Bulletins for more detailed information.</a>'
      )
    )
}
