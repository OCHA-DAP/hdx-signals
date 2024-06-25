box::use(dplyr)

#' Template data frame for initial alerts
#'
#' The required columns and typing that must be output by any `alert_{indicator}.R`
#' script. Used to validate alerts.
#'
#' @export
alerts_template_base <- dplyr$tibble(
  iso3 = character(),
  indicator_name = character(),
  indicator_source = character(),
  indicator_id = character(),
  date = as.Date(integer(0)),
  alert_level_numeric = integer(),
  value = numeric()
)

#' Template data frame for full alerts
#'
#' The required columns and typing that must be output for the full alerts after
#' generation with `alert_{indicator}.R` and adding location info to get the
#' final output from `generate_alerts.R`. Used in `generate_signals.R` on to
#' verify all columns correct.
#'
#' @export
alerts_template_full <- dplyr$bind_cols(
  alerts_template_base,
  dplyr$tibble(
    location = character(),
    region = character(),
    hrp_location = logical(),
    lat = numeric(),
    lon = numeric(),
    alert_level = character()
  )
) |>
  dplyr$relocate(
    location:lon,
    .after = iso3
  ) |>
  dplyr$relocate(
    alert_level,
    .after = alert_level_numeric
  )

#' Campaign content template
#'
#' Names and types of columns that should be generated in
#' `generate_campaign_content.R`. Used to validate the return values and also
#' to delete any campaign content using `delete_campaign_content.R`.
#'
#' @export
campaign_content_template <- dplyr$tibble(
  plot_title = character(),
  plot_id = character(),
  plot_url = character(),
  map_title = character(),
  map_id = character(),
  map_url = character(),
  plot2_title = character(),
  plot2_id = character(),
  plot2_url = character(),
  other_images_ids = character(),
  other_images_urls = character(),
  other_images_captions = character(),
  summary_long = character(),
  summary_short = character(),
  summary_source = character(),
  hdx_url = character(),
  source_url = character(),
  other_urls = character(),
  further_information = character()
)

#' Overall campaign template
#'
#' Required columns and types for campaigns that are generated from a set of content.
#' Includes `iso3` and `date` to ensure it can be joined back to the original
#' data. Used to validate the dataset as well automatically delete campaign
#' content in `delete_campaign_content.R`.
#'
#' @export
campaign_template <- dplyr$tibble(
  iso3 = character(),
  date = as.Date(integer()),
  template_id_archive = character(),
  template_id_email = character(),
  campaign_id_archive = character(),
  campaign_id_email = character(),
  campaign_url_archive = character(),
  campaign_url_email = character(),
  campaign_date = as.Date(integer()),
  signals_version = character()
)

#' Overall signals template
#'
#' Required columns and types for the full signals dataset generated for an
#' indicator. Used to create empty return value in `generate_signals()` if
#' alerts is empty.
#'
#' @export
signals_template <- dplyr$bind_cols(
  alerts_template_full,
  campaign_content_template,
  dplyr$select(campaign_template, -iso3, -date)
)

#' Signals HDX template
#'
#' Required columns and types for the signals dataset saved out to HDX. Used
#' to select and validate the columns in `triage_signals()` before saving out
#' to HDX.
#'
#' @export
signals_hdx_template <- signals_template |>
  dplyr$select(
    iso3,
    location,
    region,
    hrp_location,
    indicator_id,
    date,
    alert_level,
    value,
    plot = plot_url,
    map = map_url,
    plot2 = plot2_url,
    other_images = other_images_urls,
    summary_long,
    summary_short,
    summary_source,
    hdx_url,
    source_url,
    other_urls,
    further_information,
    campaign_url = campaign_url_archive,
    campaign_date,
    signals_version
  )
