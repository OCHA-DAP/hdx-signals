#' Format date in correct form
format_date <- function(date) {
  trimws(format(date, format = "%e %B %Y"))
}
