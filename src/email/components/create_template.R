box::use(glue)

box::use(./email_body)

#' Create template HTML from body
#'
#' Creates template HTML for uploading to Mailchimp from body text. Just reads in
#' the overall template file, which has glue string literals inside `{{body}}!`
#' to place in the `body` of the text.
#'
#' @param body String of text to go in the body.
#'
#' @returns String of the new HTML template
create_template <- function(body) {
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
