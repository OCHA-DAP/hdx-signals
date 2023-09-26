box::use(blastula)
box::use(stringr)
box::use(here)

# local modules
box::use(gs = ../utils/google_sheets)
box::use(../utils/format_date[format_date])
box::use(../utils/gmas_test_run[gmas_test_run])

#' Send out email
#'
#' Renders and sends out email. `flag_type`, `flag_source`, and `df_email` are
#' used in the .Rmd file to specifically build out the data for a specific
#' set of countries to render the email for. They are not directly referenced
#' in this function, but are referenced within email.Rmd itself.
#'
#' Note that the usage of `test_email` is different to that of `gmas_test_run()`.
#' `gmas_test_run()` is designed so that when testing that the system runs
#' properly, nothing leaves the system, so emails are not sent to anyone.
#' `test_email` is available so that it is possible to test design changes to
#' the email system by sending emails to a select set of users. This is likely
#' used in interactive sessions in the test/test.R script. If `test_email` is
#' `TRUE`, then `gmas_test_run()` is ignored and email is sent anyway.
#'
#' Sending email requires a set of environment variables to access the CHD
#' CHD credentials:
#'
#' * `CHD_DS_EMAIL_USERNAME`: server username
#' * `CHD_DS_EMAIL_PASSWORD`: server password
#' * `CHD_DS_EMAIL_HOST`: server host
#' * `CHD_DS_EMAIL_ADDRESS`: actual email address
#'
#' @param flag_type Type of flag to generate.
#' @param flag_source Source of flag for generation.
#' @param df_email Data frame for emailing, which is typically the `flags_total`
#'     dataset filtered to rows we want to generate emails for.
#' @param test_email If `TRUE`, designates the email to be a test email, putting
#'     text in the title indicating this and limiting recipients to those
#'     designated for testing.
#'
#' @returns Nothing, run to generate emails.
#'
#' @export
send_email <- function(flag_type, flag_source, df_email, test_email) {
  df_recipients <- gs$read_gs_file("email_recipients")

  # only send out to individuals designated for testing if necessary
  if (test_email) {
    df_recipients <- df_recipients[df_recipients$test,]
  }

  # extract who we are sending to and from
  to = df_recipients[df_recipients$to,]$email
  bcc = df_recipients[!df_recipients$to,]$email

  print(getwd())
  print(list.files())
  rendered_email <- blastula$render_email(
    input = here$here(
      "src",
      "email",
      "email.Rmd"
    )
  )

  if (gmas_test_run() & !test_email) {
    message(
      "`send_email()` not generating an email as `gmas_test_run()` is `TRUE`. ",
      "Set `test_email` to `TRUE` if you want `send_email()` ",
      "to send out test emails. Alternatively, set `GMAS_TEST_RUN` env variable ",
      "to `FALSE`, but be wary of saving data and pinging the OpenAI API."
    )
    return(invisible(NULL))
  }

  blastula$smtp_send(
    email = rendered_email,
    to = to,
    bcc = bcc,
    from = Sys.getenv("CHD_DS_EMAIL_ADDRESS"),
    subject = paste0(
      "Monitoring Alert: ",
      stringr$str_replace_all(stringr$str_to_title(flag_type), "_", " "),
      " - ",
      format_date(Sys.Date()),
      if (test_email) " - TEST" else ""
    ),
    credentials = email_creds
  )
}


# email credentials for sending out
email_creds <- blastula$creds_envvar(
  user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
  pass_envvar = "CHD_DS_EMAIL_PASSWORD",
  host = Sys.getenv("CHD_DS_HOST"),
  port = Sys.getenv("CHD_DS_PORT"),
  use_ssl = TRUE
)

#########################
#### OTHER UTILITIES ####
#########################

#' Add an image with custom HTML
#'
#' Creates a custom add image by using code that makes the image responsive
#' even in Windows Outlook. Based on here: https://stackoverflow.com/questions/2426072/is-there-an-equivalent-of-css-max-width-that-works-in-html-emails
#' Parameters passed directly to [blastula::add_image()]
#'
#' @export
add_image_custom <- function(
    file,
    alt = "",
    width = 520,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
) {
  # get default blastula image HTML
  html <- blastula$add_image(
    file = file,
    alt = alt,
    width = width,
    align = align,
    float = float
  )

  custom_html(
    html = html,
    width = width
  )
}

#' Add custom ggplot to email
#'
#' Use add_ggplot() to save out plots and add them back to the file
#' with custom width. Parameters passed on to [blastula::add_ggplot()] except
#' for `html_width` which specifies the max width in the custom HTML.
#'
#' @export
add_ggplot_custom <- function(
    plot_object,
    width = 5,
    height = 5,
    html_width = 1000,
    alt = NULL,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
) {
  html <- blastula$add_ggplot(
    plot_object = plot_object,
    width = width,
    height = height,
    alt = alt,
    align = align,
    float = float
  )

  custom_html(
    html = html,
    width = html_width
  )
}

#' Add custom HTML
#'
#' Finds the style blocks for images as exported from [blastula::add_image()] and
#' [blastula::add_gplot()] to then add in a custom table which is reactive even
#' on Windows Outlook.
#'
#' Used within `add_image_custom()` and `add_ggplot_custom()`
custom_html <- function(html, width) {
  img_html <- stringr$str_extract(
    html,
    "(<img src.*px;\"/>)",
    group = 1
  )
  img_html_styled <- stringr$str_replace(
    img_html,
    "(?<=style=\")(.*)(?=\"/>)",
    "display:block;width:100%"
  )

  # create the new custom table for the HTML
  cat(
    paste0(
      '<table border="0" cellspacing="0" width="100%"><tr><td></td><td width="',
      width,
      '">',
      img_html_styled,
      "</td><td></td></tr></table>"
    )
  )
}
