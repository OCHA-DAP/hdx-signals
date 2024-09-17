box::use(
  httr2,
  dplyr,
  purrr,
  readr,
  logger
)

box::use(
  src/utils/get_env,
  cs = src/utils/cloud_storage
)

#' Download raw conflict data
#'
#' Downloads raw conflict data from the ACLED API. Uses the ACLED API,
#' which requires the `ACLED_EMAIL_ADDRESS` and `ACLED_ACCESS_KEY` environment
#' variables. No longer uses the `{acled.api}` package as the downloads were
#' unsuccessful, so instead directly using `{httr2}` and the API endpoint.
#'
#' The downloaded data is stored on the Azure blob so we don't have to make multiple calls to
#' the ACLED API in a single day. To reduce the size of the returned data, only
#' relevant event types are returned and the only columns returned are the
#' numeric ISO codes, geocoordinates of the event, date of the event,
#' number of fatalities, and notes describing the event.
#'
#' @export
raw <- function() {
  # first we check that we haven't already downloaded ACLED data today, and if we
  # have, just use that raw data. Also checks that `HS_FIRST_RUN` values match
  date_check <- cs$read_az_file("output/acled_conflict/download_date.parquet")

  if (date_check$acled_download_date == Sys.Date()) {
    logger$log_debug("ACLED data already downloaded today. Using existing raw.parquet file on Azure")
    cs$read_az_file("output/acled_conflict/raw.parquet")
  } else {
    logger$log_debug("Downloading ACLED data")
    df_acled <- httr2$request(
      "https://api.acleddata.com/acled/read"
    ) |>
      httr2$req_url_query(
        key = get_env$get_env("ACLED_ACCESS_KEY"),
        email = get_env$get_env("ACLED_EMAIL_ADDRESS"),
        fields = paste(
          "iso",
          "event_date",
          "event_type",
          "latitude",
          "longitude",
          "fatalities",
          "notes",
          sep = "|"
        ),
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
      acled_download_date = Sys.Date()
    ) |>
      cs$update_az_file("output/acled_conflict/download_date.parquet")

    cs$update_az_file(df_acled, "output/acled_conflict/raw.parquet")
    df_acled
  }
}
