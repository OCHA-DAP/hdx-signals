box::use(glue)

box::use(./email_body)

#' Create template HTML from body
#'
#' Creates template HTML for uploading to Mailchimp from body text. Just reads in
#' the overall template file, which has glue string literals inside `{{body}}!`
#' to place in the `body` of the text.
#'
#' `banner_url` is used to get the correct banner for the specific indicator.
#'
#' `archive_url` is passed because campaigns with conditional logic to hide location
#' blocks are empty in the archive. So, a campaign with no conditional logic is
#' created, and that URL is used for all alerts.
#'
#' @param body String of text to go in the body.
#' @param archive_url Archive URL to be linked in the top right of the email.
#'     Defaults to the Mailchimp language that will link to the emails own
#'     archive.
#' @returns String of the new HTML template
create_template <- function(body, banner_url, archive_url = "*|ARCHIVE|*") {
  template <- readLines("src/email/components/hdx_signals_template.html", warn = FALSE) |>
    paste(
      collapse = "\n"
    )

  glue$glue(
    template,
    .open = "{{",
    .close = "}}!"
  )
}
