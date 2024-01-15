box::use(dplyr)

box::use(cs = ../utils/cloud_storage)

#' Remove recipient from recipients data frame
#'
#' Pass the name of a recipient to remove from the recipients data frame. Once
#' removal is confirmed, data frame updated on GCS. A backup is always saved
#' to mirror the previous version before these changes, just in case something
#' is erroneously removed.
#'
#' @param recipients Name(s) of the recipients to remove, matching how they are
#'     stored in `input/email_recipients.parquet`. Vectorized to remove multiple
#'     if necessary.
#'
#' @returns No return value, but `input/email_recipients.parquet` updated on
#'     the GCS.
#'
#' @export
remove_recipients <- function(recipients) {
  # read the recipients list
  df_recipients <- cs$read_gcs_file("input/email_recipients.parquet")

  # check that all recipients to remove are in the GCS file
  recip_check <- recipients %in% df_recipients$recipient

  if (!all(recip_check)) {
    missing_recips <- paste(recipients[!recip_check], collapse = ", ")
    stop(
      "The following recipients are not in `input/email_recipients.parquet`: ",
      missing_recips,
      ". Check spelling.",
      call. = FALSE
    )
  }

  confirm_removal <- readline(
    paste0(
      "Type 'Y' to confirm REMOVAL of ",
      paste(recipients, collapse = ", "),
      ". "
    )
  )

  if (tolower(confirm_removal) != "y") {
    stop(
      "You did not confirm recipient removal, so `input/email_recipients.parquet` ",
      "has not been updated.",
      call. = FALSE
    )
  }

  df_recipients_new <- dplyr$filter(df_recipients, !(recipient %in% recipients))
  cs$update_gcs_file(df_recipients, "input/email_recipients_backup.parquet")
  cs$update_gcs_file(df_recipients_new, "input/email_recipients.parquet")
}

#' Add recipients to recipients data frame
#'
#' Pass the name of recipient(s) to remove from the recipients data frame. Once
#' addition is confirmed, data frame updated on GCS. A backup is always saved
#' to mirror the previous version before these changes, just in case something
#' is erroneously added.
#'
#' There is no `test` or `to` parameters, because recipients should almost never
#' be added to the `test` mailing list or included in the `to` field on emails.
#' You will have to manually update the database to do this. All additions with
#' this function get values of `FALSE` for both `test` and `to`.
#'
#' @param recipients Name(s) of the recipients to add. Length must match that
#'     of `emails`.
#' @param emails Email(s) of the recipients to add. Length must match that of
#'     of `recipients`.
#'
#' @returns No return value, but `input/email_recipients.parquet` updated on
#'     the GCS.
#'
#' @export
add_recipients <- function(recipients, emails) {
  # read the recipients list
  df_recipients <- cs$read_gcs_file("input/email_recipients.parquet")

  # check that length of emails matches length of names

  if (length(recipients) != length(emails)) {
    stop(
      "`recipients` and `emails` must be vectors of the same length.",
      call. = FALSE
    )
  }

  confirm_addition <- readline(
    paste0(
      "Type 'Y' to confirm ADDITION of ",
      paste(recipients, emails, collapse = ", ", sep = ": "),
      ". "
    )
  )

  if (tolower(confirm_addition) != "y") {
    stop(
      "You did not confirm recipient addition, so `input/email_recipients.parquet` ",
      "has not been updated.",
      call. = FALSE
    )
  }

  df_recipients_new <- dplyr$bind_rows(
    df_recipients,
    data.frame(
      recipient = recipients,
      email = emails,
      test = FALSE,
      to = FALSE
    )
  )

  cs$update_gcs_file(df_recipients, "input/email_recipients_backup.parquet")
  cs$update_gcs_file(df_recipients_new, "input/email_recipients.parquet")
}

