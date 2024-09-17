#' Check if this run is intended to be the first run for an indicator
#'
#' `hs_first_run()` looks for the environment variable `HS_FIRST_RUN`. This is
#' used in `generate_signals()` and helper functions to determine if this is
#' the first run for an indicator to generate a record of historical signals.
#'
#' If `HS_FIRST_RUN` is not set, it defaults to `FALSE`. This means it is
#' expected to be monitoring that data. `check_existing_signals()` makes use
#' of the env variable to check that there is already a record of historical
#' signals if `FALSE`, and that there are no existing signals if `TRUE`.
#'
#' @export
hs_first_run <- function() {
  as.logical(Sys.getenv(x = "HS_FIRST_RUN", unset = FALSE))
}
