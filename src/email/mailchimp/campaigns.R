box::use(httr2)

box::use(./base_api)

#' Adds a campaign to Mailchimp
#'
#' Adds a campaign to Mailchimp, using the default settings for HDX Signals, and
#' adding that campaign to the HDX Signals folder. Returns the ID of the campaign
#'
#' @param subject_line Subject of the email
#' @param preview_text Preview text for the email
#' @param title Title of the email
#' @param recipients Recipients list for the email
#' @param template_id ID of the template to be used in the email
#'
#' @export
mc_add_campaign <- function(subject_line, preview_text, title, recipients, template_id) {
  response <- base_api$mc_api(lists_api = FALSE) |>
    httr2$req_url_path_append(
      "campaigns"
    ) |>
    httr2$req_body_json(
      data = list(
        type = "regular",
        recipients = recipients,
        settings = list(
          title = title,
          subject_line = subject,
          preview_text = preview_text,
          from_name = "HDX Signals",
          reply_to = "seth.caldwell@un.org",
          template_id = template_id,
          folder_id = "56104f0a36"
        )
      )
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json()

  response$id
}

#' Send Mailchimp campaign
#'
#' Sends Mailchimp campaign.
#'
#' @param campaign_id ID of campaign to send
#'
#' @returns Nothing
#'
#' @export
mc_send_campaign <- function(campaign_id) {
  base_api$mc_api(lists_api = FALSE) |>
    httr2$req_url_path_append(
      "campaigns",
      campaign_id,
      "actions",
      "send"
    ) |>
    httr2$req_method("POST") |>
    httr2$req_perform()
}
