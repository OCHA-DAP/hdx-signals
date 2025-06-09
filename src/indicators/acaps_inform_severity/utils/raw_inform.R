box::use(
  httr2,
  dplyr,
  purrr
)

box::use(
  src/utils/get_env,
  cs = src/utils/cloud_storage
)


#' Download raw INFORM severity index data
#'
#' Downloads raw conflict data from the ACAPS API. Uses the ACAPS credentials,
#' which requires the `ACAPS_TOKEN` environment variables.
#'
#' The downloaded data is stored on the Azure blob so we don't have to make multiple calls to
#' the ACAPS API in a single day. .
#'
#' @export
raw <- function() {
  date_check <- cs$read_az_file("output/acaps_inform_severity/download_date.parquet")
  if (Sys.Date() > date_check$acaps_download_date) {
    df <- cs$read_az_file("output/acaps_inform_severity/raw.parquet")
    if (max(df$date) < as.Date(format(Sys.Date(), "%Y-%m-01"))) {
      df <- get_acaps_severity()
      # upload full data frame to cloud
      cs$update_az_file(df, "output/acaps_inform_severity/raw.parquet")
    }
  } else {
    df <- cs$read_az_file("output/acaps_inform_severity/raw.parquet")
  }
  df
}


#' Gets ACAPS severity data for a specific date
#'
#' Requests ACAPS severity data for the specific `formatted_date`. The API is
#' called using the `ACAPS_TOKEN` env variable which should be your personal
#' usage token for ACAPS. Requests are retried 5 times maximum, backing off slightly
#' longer each tyr to avoid throttling.
#'
#' Only specific portions of the response are returned in a formatted data frame.
#' Amend that portion of the code to extract additional portions of the returned
#' JSON file.
#'
#' If the API returns an error for the latest month, then we assume that the data
#' is not yet available, and silently return an empty data frame. If the error
#' is generated for any previous month, then the function fails.
#'
#' @param formatted_date Date formatted as `MonYYYY`, such as `Jan2020`.
get_acaps_severity_date <- function(formatted_date) {
  base_url <- "https://api.acaps.org/api/v1/inform-severity-index/"
  token <- Sys.getenv("ACAPS_TOKEN")

  data <- fetch_all_pages(base_url, token, formatted_date)
  data
}

#' Get ACAPS severity data
#'
#' Gets ACAPS severity data from all of the specific date endpoints from January 2019
#' to today's date. Pulls the resulting data into a single data frame arranged by
#' ISO3 and date.
get_acaps_severity <- function() {
  formatted_dates <- seq.Date(from = as.Date("2019-01-01"), to = Sys.Date(), by = "month") |>
    format("%b%Y")

  purrr$map(
    .x = formatted_dates,
    .f = get_acaps_severity_date
  ) |>
    purrr$list_rbind() |>
    dplyr$arrange(
      iso3,
      date
    )
}

fetch_all_pages <- function(base_url, token, formatted_date) {
  all_results <- list()
  next_url <- base_url
  while (!is.null(next_url)) {
    print(formatted_date)
    if (next_url == base_url) {
      # Append the formatted_date only to the base_url
      request <- httr2$request(next_url) |>
        httr2$req_url_path_append(formatted_date)
    } else {
      # Use the next_url directly without appending formatted_date again
      request <- httr2$request(next_url)
    }
    response <- request |>
      httr2$req_headers(Authorization = paste("Token", token)) |>
      httr2$req_retry(max_tries = 5, backoff = \(x) x / 10) |>
      httr2$req_error(is_error = \(resp) {
        if (format(Sys.Date(), "%b%Y") == formatted_date || !httr2$resp_is_error(resp)) FALSE else TRUE
      }) |>
      httr2$req_perform() |>
      httr2$resp_body_json()

    # Extract and process the current page's results
    page_results <- response$results |>
      purrr$map(\(x) dplyr$tibble(
        iso3 = unlist(x$iso3),
        country = unlist(x$country),
        regions = unlist(x$regions),
        crisis_id = x$crisis_id,
        crisis_name = x$crisis_name,
        inform_severity_index = x$`INFORM Severity Index`,
        impact_crisis = x$`Impact of the crisis`,
        people_condition = x$`Conditions of affected people`,
        complexity = x$Complexity,
        drivers = paste(x$drivers, collapse = ", "),
        date = as.Date(x$`_internal_filter_date`),
        reliability = x$reliability,
        reliability_score = x$reliability_score,
        country_level = x$country_level
      ))

    all_results <- append(all_results, page_results)

    # Update the next_url for the next iteration
    next_url <- response[["next"]]
  }

  # Combine all results into a single tibble
  dplyr$bind_rows(all_results)
}


