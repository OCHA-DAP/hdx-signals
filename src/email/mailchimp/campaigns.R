box::use(
  httr2,
  logger
)

box::use(
  src/email/mailchimp/base_api,
  src/email/mailchimp/folders,
  src/utils/hs_local,
  src/utils/get_env,
  src/utils/hs_logger
)

hs_logger$configure_logger()

#' Adds a campaign to Mailchimp
#'
#' Adds a campaign to Mailchimp, using the default settings for HDX Signals, and
#' adding that campaign to the HDX Signals folder. Returns the URL of the campaign.
#'
#' @param subject_line Subject of the email
#' @param preview_text Preview text for the email
#' @param title Title of the email
#' @param recipients Recipients list for the email
#' @param template_id ID of the template to be used in the email
#'
#' @returns ID and URL of the campaign
#'
#' @export
mc_add_campaign <- function(subject_line, preview_text, title, recipients, template_id, folder) {
  if (hs_local$hs_local()) {
    logger$log_debug(
      "Since `hs_local()`, no campaign added to Mailchimp."
    )
    list(id = "test-id", url = "test-url")
  } else {
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
            subject_line = subject_line,
            preview_text = preview_text,
            from_name = "HDX Signals",
            reply_to = get_env$get_env("HS_EMAIL"),
            template_id = as.numeric(template_id),
            folder_id = folders$mc_campaign_folder_id(folder)
          )
        )
      ) |>
      httr2$req_perform() |>
      httr2$resp_body_json()

    list(id = as.character(response$id), url = response$archive_url)
  }
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
  req <- base_api$mc_api(lists_api = FALSE) |>
    httr2$req_url_path_append(
      "campaigns",
      campaign_id,
      "actions",
      "send"
    ) |>
    httr2$req_method("POST")

  if (hs_local$hs_local()) {
    logger$log_debug(
      "Since `hs_local()`, no campaign sent, dry run returned."
    )
    httr2$req_dry_run(req)
  } else {
    httr2$req_perform(req)
  }
}
