box::use(dplyr)

box::use(./components/email_body)
box::use(./components/create_template)
box::use(./mailchimp/templates)
box::use(./mailchimp/campaigns)
box::use(./mailchimp/custom_segmentation)
box::use(../utils/format_date[format_date])

#' Generate and send campaigns
#'
#' Generates and sends Mailchimp campaign(s). The way segmentation works,
#' email campaigns are sent separately to the different values of `alert_level`
#' for a specific `indicator_id`. Thus, this calls `generate_campaign()` for
#' each unique value of `alert_level`.
#'
#' This function is used directly in the indicator scripts. This allows a single
#' function call for any new alerts. If there are `Medium concern` alerts in
#' `alerts_df$alert_level`, then the URL of `Medium concern` is always returned,
#' even if a separate campaign is created for `High concern`. This is because
#' all relevant countries/alerts will be included in the campaign, so the URL
#' can cover all countries.
#'
#' @param indicator_id ID of the indicator, used for segmentation. {provider}_{type}
#'     is the standard format, such as `idmc_displacement`.
#' @param shock_title Shock title text to go next to HDX Signals at the top of the email.
#' @param template_folder Name of the folder to store the Mailchimp template in.
#' @param alerts_df Data frame with alerts information for generating the email.
#' @param send_email Whether or not to send an email for the campaign. If `TRUE`,
#'     calculates segmentation normally
#'
#' @returns URL of the campaign. If two campaigns sent, then returns the URL
#' for `Medium concern` since it will contain all country alerts for that run.
#'
#' @export
generate_campaigns <- function(
    indicator_id,
    shock_title,
    template_folder,
    alerts_df,
    send_email = TRUE
) {
  # generate email campaign with no conditional logic for the archive
  archive_url <- generate_campaign(
    indicator_id = indicator_id,
    shock_title = shock_title,
    template_folder = template_folder,
    alert_level = "High concern",
    alerts_df = alerts_df,
    archive = TRUE,
    archive_url = "*|ARCHIVE|*"
  )

  if (send_email && "High concern" %in% alerts_df$alert_level) {
    # only high concern alerts for the high concern crowd
    url <- generate_campaign(
      indicator_id = indicator_id,
      shock_title = shock_title,
      template_folder = template_folder,
      alert_level = "High concern",
      alerts_df = dplyr$filter(alerts_df, alert_level == "High concern"),
      archive = FALSE,
      archive_url = archive_url
    )
  } else if (send_email && "Medium concern" %in% alerts_df$alert_level) {
    url <- generate_campaign(
      # all alerts goes to the medium concern crowd
      indicator_id = indicator_id,
      shock_title = shock_title,
      template_folder = template_folder,
      alert_level = "Medium concern",
      alerts_df = alerts_df,
      archive = FALSE,
      archive_url = archive_url
    )
  }
  archive_url
}

#' Generate and send a campaign
#'
#' Generates and sends a Mailchimp campaign. First, an email body is created
#' and used to generate a new Mailchimp template for the campaign. Segmentation
#' is done based on the indicator and countries, and the empty segment is used
#' if this is an historical campaign. Finally, a new campaign is created in
#' Mailchimp and sent out.
#'
#' @param indicator_id ID of the indicator, used for segmentation. {provider}_{type}
#'     is the standard format, such as `idmc_displacement`.
#' @param shock_title Shock title text to go next to HDX Signals at the top of the email
#' @param template_folder Name of the template folder to store the templates in
#' @param alert_level Level of alert to send out. Used for segmentation.
#' @param alerts_df Data frame with alerts information for generating the email.
#' @param archive Whether or not this email is designed for the archive. If `TRUE`,
#'     no conditional logic is included in the email, and the email is previewed
#'     if in a test run. If `FALSE`, conditional logic is used and emails are
#'     sent to segments.
#' @param archive_url URL for the archive.
#'
#' @returns URL of the campaign
generate_campaign <- function(
    indicator_id,
    shock_title,
    template_folder,
    alert_level,
    alerts_df,
    archive,
    archive_url
) {

  # create campaign information
  subject <- paste0("HDX Signals: ", shock_title, ", ", format_date(Sys.Date()))
  title <- paste(
    tolower(shock_title),
    paste(alerts_df$iso3, collapse = "_"),
    if (archive) "archive" else "live",
    format(Sys.Date(),"%Y_%m_%d"),
    sep = "_"
  )

  # create the email template and add to Mailchimp
  body <- email_body$create_body(
    shock_title = shock_title,
    iso3 = alerts_df$iso3,
    country = alerts_df$country,
    message = alerts_df$message,
    plot = alerts_df$plot,
    map = alerts_df$map,
    other_images = alerts_df$other_images,
    summary = alerts_df$summary,
    further_information = alerts_df$further_information,
    use_conditions = !archive
  )

  template <- create_template$create_template(
    body = body,
    archive_url = archive_url
  )
  template_id <- templates$mc_add_template(
    html = template,
    folder = template_folder,
    preview = archive # only preview the archive template, not those with conditonal logic
  )

  # create segmentation
  if (!archive) {
    segments <- custom_segmentation$mc_segment_conditions(
      indicator_id = indicator_id,
      iso3 = alerts_df$iso3,
      alert_level = alert_level
    )
  } else {
    # assign to empty segment for historic campaigns so URL created but no emails sent
    segments <- custom_segmentation$mc_empty_segment()
  }

  id_url <- campaigns$mc_add_campaign(
    subject_line = subject,
    preview_text = paste(alerts_df$country, collapse = ", "),
    title = title,
    recipients = segments,
    template_id = template_id
  )

  if (!archive) campaigns$mc_send_campaign(id_url[["id"]])

  id_url[["url"]]
}
