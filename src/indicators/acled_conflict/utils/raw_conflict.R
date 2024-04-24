box::use(acled.api)
box::use(lubridate)

#' Download raw conflict data
#'
#' Downloads raw conflict data from the ACLED API. Uses the `{acled.api}` package,
#' which requires the `ACLED_EMAIL_ADDRESS` and `ACLED_ACCESS_KEY` environment
#' variables.
#'
#' @param first_run If `TRUE`, downloads all data. Otherwise, it only loads in 1500
#'      days prior to the current date, more than enough to generate any recent
#'      signals.
#'
#' @export
raw <- function(first_run = FALSE) {
  if (first_run) {
    start.date <- "2018-01-01"
    end.date <- as.character(Sys.Date())
  } else {
    start.date <- as.character(Sys.Date() - lubridate$days(1500))
    end.date <- as.character(Sys.Date()) # have to pass end date if you pass `start.date`
  }

  acled.api$acled.api(
    start.date = start.date,
    end.date = end.date,
    add.variables = c("notes", "latitude", "longitude")
  )
}
