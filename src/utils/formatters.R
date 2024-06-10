box::use(dplyr)

#' Format date in correct form
#'
#' Formats date to have the 8 August 2023 format: no leading zero on the day,
#' full month name, and full year. Also trims the whitespace around the date
#'
#' @param date Date to format.
#'
#' @export
format_date <- function(date) {
  trimws(format(date, format = "%e %B %Y"))
}
