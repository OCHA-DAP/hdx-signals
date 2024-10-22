box::use(
  httr2,
  dplyr,
  purrr,
  readr,
  logger,
)

box::use(
  src/utils/get_env,
  cs = src/utils/cloud_storage
)


#' Download raw INFORM severity index data
#'
#' Downloads raw conflict data from the ACAPS API. Uses the ACAPS credentials,
#' which requires the `ACAPS_EMAIL_ADDRESS` and `ACAPS_PWD` environment
#' variables.
#'
#' The downloaded data is stored on the Azure blob so we don't have to make multiple calls to
#' the ACAPS API in a single day. .
#'
#' @export
raw <- function() {
  # first we check that we haven't already downloaded ACAPS data today, and if we
  # have, just use that raw data. Also checks that `HS_FIRST_RUN` values match
  date_check <- cs$read_az_file("output/acaps_inform/download_date.parquet")

  if (date_check$acaps_download_date == Sys.Date()) {
     logger$log_debug("ACAPS data already downloaded today. Using existing raw.parquet file on Azure")
     cs$read_az_file("output/acaps_inform/raw.parquet")
  } else {
    credentials <- list(username=get_env$get_env("ACAPS_EMAIL_ADDRESS"), password=get_env$get_env("ACAPS_PWD"))
    auth_token_response <- httr2$request("https://api.acaps.org/api/v1/token-auth/") |>
      httr2$req_body_json(credentials) |>
      httr2$req_perform()

    auth_token <- httr2$resp_body_json(auth_token_response)$token

    #Get current month and year
    date_api=format(Sys.Date(), format = '%b%Y')
    ## Wait to avoid throttling

    df <- data.frame()
    request_url <- paste0("https://api.acaps.org/api/v1/inform-severity-index/",date_api)
    last_request_time <- Sys.time()

    while (TRUE) {

      # Wait to avoid throttling
      while (as.numeric(Sys.time() - last_request_time) < 1) {
        Sys.sleep(0.1)
      }

      # Make the request with verbose logging
      response <- httr2$request(request_url) |>
        httr2$req_headers(
          Authorization = paste("Token", auth_token)
        ) |>
        httr2$req_perform() |>
        httr2$resp_body_json()

      last_request_time <- Sys.time()

      # Extract the data and convert any list-type columns to strings
      df_results <- dplyr$tibble(
        iso3 = sapply(response$results, `[[`, "iso3"),
        country = sapply(response$results, `[[`, "country"),
        regions = sapply(response$results, `[[`, "regions"),
        crisis_id = sapply(response$results, `[[`, "crisis_id"),
        crisis_name = sapply(response$results, `[[`, "crisis_name"),
        inform_severity_index = sapply(response$results, `[[`, "INFORM Severity Index")
      )

      # Append to the main dataframe
      f <- rbind(df, df_results)

      # Loop to the next page; if we are on the last page, break the loop
      next_url <- resp_body_json(response)["next"]
      if (!is.null(next_url)) {
        request_url <- next_url
      } else {
        break
      }
    }
    purrr$pluck("data") |>
    purrr$map(dplyr$as_tibble) |>
    purrr$list_rbind() |>
    readr$type_convert(
      col_types = readr$cols())

    # Add download date
    dplyr$tibble(
    acaps_download_date = Sys.Date()
    ) |>
    cs$update_az_file("output/acaps_inform/download_date.parquet")

    cs$update_az_file(df_inform, "output/acaps_inform/raw.parquet")
    df_inform
  }
}

raw()
