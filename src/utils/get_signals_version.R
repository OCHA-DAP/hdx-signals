#' Get Signals version
#'
#' Returns the Signals version stored in `.signals-version` as a character
#' string.
#'
#' @returns Signals version
#'
#' @export
get_signals_version <- function() {
  readLines(".signals-version")
}
