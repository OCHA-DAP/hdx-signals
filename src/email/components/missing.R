#' Checks for missing text
#' @export
missing_text <- function(x) {
  is.null(x) || is.na(x) || x == ""
}
