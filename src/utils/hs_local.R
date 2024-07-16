#' Checks if this run is to be executed without external connections
#' to MailChimp and OpenAI
#'
#' `hs_local()` looks for the environment variable `HS_LOCAL`. This is
#' used in other functions so emails are not sent and data not saved back to the
#' Azure storage container so test runs do not have any impact on the stored
#' data and do not send out emails.
#'
#' By default, if `HS_LOCAL` is not set, then it defaults to `TRUE`. This
#' means in interactive development sessions you do not need to worry about
#' accidentally running something. It is used in:
#' * `ai_summarizer()` to return a standard string instead of pinging the OpenAI
#'     API for summarization.
#' * `update_az_file()` to prevent saving data back to the Google Cloud Storage
#'     buckets.
#'
#' @export
hs_local <- function() {
  as.logical(Sys.getenv("HS_LOCAL", unset = TRUE))
}
