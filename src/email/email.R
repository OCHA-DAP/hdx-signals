box::use(./components/email_body)
box::use(./components/create_template)
box::use(./mailchimp/templates)
box::use(./mailchimp/campaigns)
box::use(./mailchimp/custom_segmentation)
box::use(../utils/format_date[format_date])


#'
campaign <- function(
    indicator_id,
    signal_level,
    shock_title,
    iso3,
    country,
    message = "",
    plot = "",
    map = "",
    other_images = "",
    summary = "",
    further_information = ""
) {

  # create the email template and add to Mailchimp
  body <- email_body$create_body(
    shock_title = shock_title,
    iso3 = iso3,
    country = country,
    message = message,
    plot = plot,
    map = map,
    other_images = other_images,
    summary = summary,
    further_information = further_information
  )
  template <- create_template$create_template(body)
  template_id <- templates$mc_add_template(
    template
  )

  # create segmentation
  segments <- custom_segmentation$mc_segment_conditions(
    indicator_id = indicator_id,
    iso3 = iso3,
    signal_level = signal_level
  )

  # create campaign
  subject <- paste0("HDX Signals: ", shock_title, ", ", format_date(Sys.Date()))
  campaigns$mc_add_campaign(
    subject_line = subject,
    preview_text = paste(country, collapse = ", "),
    title = subject,
    recipients = segments,

  )
}
