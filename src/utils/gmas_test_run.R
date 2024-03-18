#' Checks if this run is a test run
#'
#' `gmas_test_run()` looks for the environment variable `GMAS_TEST_RUN`. This is
#' used in other functions so emails are not sent and data not saved back to the
#' Google Cloud Storage bucket so test runs do not have any impact on the stored
#' data and do not send out emails.
#'
#' By default, if `GMAS_TEST_RUN` is not set, then it defaults to `TRUE`. This
#' means in interactive development sessions you do not need to worry about
#' accidentally running something. It is used in:
#' * `ai_summarizer()` to return a standard string instead of pinging the OpenAI
#'     API for summarization.
#' * `update_az_file()` to prevent saving data back to the Google Cloud Storage
#'     buckets.
#'
#' @export
gmas_test_run <- function() {
  as.logical(Sys.getenv("GMAS_TEST_RUN", unset = TRUE))
}
