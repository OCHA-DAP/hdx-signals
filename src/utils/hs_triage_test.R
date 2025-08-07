#' Checks if triage operations should run in test mode
#'
#' `hs_triage_test()` looks for the environment variable `TEST`. This is
#' used in `triage_signals()` to determine whether to save data to production
#' files or run in test mode.
#'
#' By default, if `TEST` is not set, then it defaults to `TRUE`. This
#' means in interactive development sessions signals will not be moved to
#' the core `output/signals.parquet` file. It is used in:
#' * `dispatch_signals()` to prevent saving triaged signals to production files
#' * Test mode allows campaign operations (send/delete) but prevents file modifications
#'
#' @export
hs_triage_test <- function() {
  test_val <- Sys.getenv("TEST", unset = "TRUE")
  as.logical(test_val)
}
