box::use(dplyr)

#' Template data frame for initial alerts
#'
#' The required columns and typing that must be output by any `alert_{indicator}.R`
#' script. Used to validate alerts.
#'
#' @export
alerts_template_base <- dplyr$tibble(
  iso3 = NA_character_,
  indicator_name = NA_character_,
  indicator_source = NA_character_,
  indicator_id = NA_character_,
  date = as.Date(integer(0), origin = "1970-01-01"),
  alert_level_numeric = NA_integer_,
  value = NA_real_
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
    location = NA_character_,
    region = NA_character_,
    hrp_location = NA,
    lat = NA_real_,
    lon = NA_real_,
    alert_level = NA_character_
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
  plot_title = NA_character_,
  plot_id = NA_character_,
  plot_url = NA_character_,
  map_title = NA_character_,
  map_id = NA_character_,
  map_url = NA_character_,
  plot2_title = NA_character_,
  plot2_id = NA_character_,
  plot2_url = NA_character_,
  other_images_ids = NA_character_,
  other_images_urls = NA_character_,
  other_images_captions = NA_character_,
  summary_long = NA_character_,
  summary_short = NA_character_,
  hdx_url = NA_character_,
  source_url = NA_character_,
  other_urls = NA_character_,
  further_information = NA_character_
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
  iso3 = NA_character_,
  date = as.Date(x = integer(0), origin = "1970-01-01"),
  template_id_archive = NA_character_,
  template_id_email = NA_character_,
  campaign_id_archive = NA_character_,
  campaign_id_email = NA_character_,
  campaign_url_archive = NA_character_,
  campaign_url_email = NA_character_,
  campaign_date = as.Date(x = integer(0), origin = "1970-01-01")
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
