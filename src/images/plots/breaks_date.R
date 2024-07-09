box::use(lubridate)
box::use(scales)

#' Generate breaks for date axis
#'
#' From the date axis, checks the number of years covered. If 3 or more years
#' are included in the data, then the breaks are set to be the years. Otherwise,
#' `scales::breaks_pretty()` is used. We check the max compared to min because
#' it is not always the raw vector passed in, but sometimes the limits of the
#' function.
#'
#' @param x Vector to generate breaks for
#'
#' @export
breaks_date <- function(x) {
  max_year <- lubridate$year(max(x))
  min_year <- lubridate$year(min(x))
  n_years <- max_year - min_year
  if (n_years >= 2) {
    scales$breaks_width(width = "1 year")(x)
  } else {
    scales$breaks_pretty()(x)
  }
}
