box::use(
  httr2,
  dplyr,
  purrr,
  readr,
  logger,
  tidyr
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
  start_date=as.Date("2019-01-01")
  # first we check that we haven't already downloaded ACAPS data today, and if we
  # have, just use that raw data. Also checks that `HS_FIRST_RUN` values match
  date_check <- cs$read_az_file("output/acaps_inform/download_date.parquet")

  # if (date_check$acaps_download_date == Sys.Date()) {
  #    logger$log_debug("ACAPS data already downloaded today. Using existing raw.parquet file on Azure")
  #    cs$read_az_file("output/acaps_inform/raw.parquet")
  # } else {
  credentials <- list(username=get_env$get_env("ACAPS_EMAIL_ADDRESS"), password=get_env$get_env("ACAPS_PWD"))
  auth_token_response <- httr2$request("https://api.acaps.org/api/v1/token-auth/") |>
    httr2$req_body_json(credentials) |>
    httr2$req_perform()

  auth_token <- httr2$resp_body_json(auth_token_response)$token

  #Get current month and year
  end_date=Sys.Date()
  #Get all months-year combinations to be downloaded
  date_seq <- seq(from = start_date, to = end_date, by = "month")
  # Format the dates in 'MMM YYYY' format
  formatted_dates <- format(date_seq, "%b%Y")

  ## Wait to avoid throttling
  df <- data.frame()
  last_request_time <- Sys.time()

  for (date_api in formatted_dates){
    request_url <- paste0("https://api.acaps.org/api/v1/inform-severity-index/",date_api,"/?country_level=Yes")
    # # Wait to avoid throttling
    # while (as.numeric(Sys.time() - last_request_time) < 1) {
    #   Sys.sleep(0.1)
    # }

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
      inform_severity_index = sapply(response$results, `[[`, "INFORM Severity Index"),
      impact_crisis = sapply(response$results, `[[`,"Impact of the crisis"),
      people_condition = sapply(response$results, `[[`,"Conditions of affected people"),
      complexity = sapply(response$results, `[[`,"Complexity"),
      drivers = sapply(response$results, `[[`, "drivers"),
      date = sapply(response$results, `[[`, "_internal_filter_date")
    )

    # Append to the main dataframe
    df <- rbind(df, df_results)
  }
  df |>
    dplyr::mutate(
      drivers = purrr.map_chr(
        .x = drivers,
        .f = \(x) paste(unlist(x), collapse = ", ")
      )
    ) |>
    tidyr.unnest(
      cols = dplyr.where(is.list)
    )

  # Add download date
  dplyr$tibble(
  acaps_download_date = Sys.Date()
  ) |>
  cs$update_az_file("output/acaps_inform/download_date.parquet")

  cs$update_az_file(df, "output/acaps_inform/raw.parquet")
  df
  # }
}

raw()
