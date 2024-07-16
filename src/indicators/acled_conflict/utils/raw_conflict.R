box::use(httr2)
box::use(lubridate)
box::use(dplyr)
box::use(purrr)
box::use(readr)
box::use(logger[log_info, log_debug, log_error])

box::use(../../../utils/hs_logger)
box::use(cs = ../../../../src/utils/cloud_storage)
box::use(../../../utils/get_env[get_env])

hs_logger$configure_logger()

#' Download raw conflict data
#'
#' Downloads raw conflict data from the ACLED API. Uses the ACLED API,
#' which requires the `ACLED_EMAIL_ADDRESS` and `ACLED_ACCESS_KEY` environment
#' variables. No longer uses the `{acled.api}` package as the downloads were
#' unsuccessful, so instead directly using `{httr2}` and the API endpoint.
#'
#' Data is downloaded from 2018 onward if `first_run`, otherwise `1500` days
#' from the current `Sys.Date()` to reduce the size of the download. The downloaded
#' data is stored on the Azure blob so we don't have to make multiple calls to
#' the ACLED API in a single day. To reduce the size of the returned data, only
#' relevant event types are returned and the only columns returned are the
#' numeric ISO codes, geocoordinates of the event, date of the event,
#' number of fatalities, and notes describing the event.
#'
#' @param first_run If `TRUE`, downloads all data. Otherwise, it only loads in 1500
#'      days prior to the current date, more than enough to generate any recent
#'      signals.
#'
#' @export
raw <- function(first_run = FALSE) {
  # first we check that we haven't already downloaded ACLED data today, and if we
  # have, just use that raw data. Also checks that `first_run` values match
  date_check <- cs$read_az_file("output/acled_conflict/download_date.parquet")
  if (date_check$acled_download_date == Sys.Date() && first_run == date_check$first_run) {
    log_debug("ACLED data already downloaded today. Using existing raw.parquet file on Azure")
    cs$read_az_file("output/acled_conflict/raw.parquet")
  } else {
    start_date <- "2018-01-01"

    log_debug(paste0("Downloading ACLED data since ", start_date))
    df_acled <- httr2$request(
      "https://api.acleddata.com/acled/read"
    ) |>
      httr2$req_url_query(
        key = get_env("ACLED_ACCESS_KEY"),
        email = get_env("ACLED_EMAIL_ADDRESS"),
        timestamp = start_date,
        fields = paste(
          "iso",
          "event_date",
          "latitude",
          "longitude",
          "fatalities",
          "notes",
          sep = "|"
        ),
        event_type = c("Battles:OR:event_type=Violence against civilians:OR:event_type=Explosions/Remote violence"),
        limit = 0 # much faster than using limits / pagination sadly
      ) |>
      httr2$req_perform() |>
      httr2$resp_body_json() |>
      purrr$pluck("data") |>
      purrr$map(dplyr$as_tibble) |>
      purrr$list_rbind() |>
      readr$type_convert(
        col_types = readr$cols()
      )

    # since the ACLED API takes significant amount of time to call
    # store the date we've downloaded so we don't continually call it each time
    # on the same day
    dplyr$tibble(
      acled_download_date = Sys.Date(),
      first_run = first_run
    ) |>
      cs$update_az_file("output/acled_conflict/download_date.parquet")

    cs$update_az_file(df_acled, "output/acled_conflict/raw.parquet")
    df_acled
  }
}
