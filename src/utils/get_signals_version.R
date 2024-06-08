#' Get Signals version
#'
#' Returns the Signals version stored in `.signals-version` as a numeric
#' version object.
#'
#' @returns Signals version
#'
#' @export
get_signals_version <- function() {
  readLines(".signals-version") |>
    as.numeric_version()
}
