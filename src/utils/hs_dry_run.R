#' Checks if this is a dry run for interactive development and testing
#'
#' `hs_dry_run()` looks for the environment variable `HS_DRY_RUN`. If `TRUE`,
#' the default, then `generate_signals()` will generate signals even if there
#' are no new signals available. This enables easier interactive development and
#' testing of visual changes. Alongside `hs_local()`, you can do local dry runs
#' or even test Mailchimp or OpenAI functionality by setting `hs_local()` to
#' `FALSE`.
#'
#' By default, if `HS_DRY_RUN` is not set, then it defaults to `TRUE`. This
#' means in interactive development sessions you do not need to worry about
#' accidentally running monitoring. In general, you should not need to set this
#' to `FALSE`, as all monitoring should be done through GitHub Actions.
#'
#' @export
hs_dry_run <- function() {
  as.logical(x = Sys.getenv("HS_DRY_RUN", unset = TRUE))
}
