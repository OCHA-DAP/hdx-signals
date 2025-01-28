box::use(
  httr2,
  dplyr,
  purrr,
  tidyr
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
    df <- get_acaps_severity()

    # update download date
    dplyr$tibble(
      acaps_download_date = Sys.Date()
    ) |>
      cs$update_az_file("output/acaps_inform_severity/download_date.parquet")

    # upload full data frame to cloud
    cs$update_az_file(df, "output/acaps_inform_severity/raw.parquet")
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
  df_country_crises<-httr2$request("https://api.acaps.org/api/v1/inform-severity-index/") |>
    httr2$req_url_path_append(
      formatted_date
    ) |>
    httr2$req_url_query(
      country_level = "Yes"
    ) |>
    httr2$req_headers(
      Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
    ) |>
    httr2$req_retry(
      max_tries = 5,
      backoff = \(x) x / 10
    ) |>
    httr2$req_error( # only return an error if not in the latest month
      is_error = \(resp) if (format(Sys.Date(), "%b%Y") == formatted_date || !httr2$resp_is_error(resp)) FALSE else TRUE
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck(
      "results"
    ) |>
    purrr$map(
      .f = \(x) {
        dplyr$tibble(
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
          date = as.Date(x$`_internal_filter_date`)
        )
      }
    ) |>
    purrr$list_rbind()

  df_all_crises<-httr2$request("https://api.acaps.org/api/v1/inform-severity-index/") |>
    httr2$req_url_path_append(
      formatted_date
    ) |>
    httr2$req_url_query(
    ) |>
    httr2$req_headers(
      Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
    ) |>
    httr2$req_retry(
      max_tries = 5,
      backoff = \(x) x / 10
    ) |>
    httr2$req_error( # only return an error if not in the latest month
      is_error = \(resp) if (format(Sys.Date(), "%b%Y") == formatted_date || !httr2$resp_is_error(resp)) FALSE else TRUE
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck(
      "results"
    ) |>
    purrr$map(
      .f = \(x) {
        dplyr$tibble(
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
          date = as.Date(x$`_internal_filter_date`)
        )
      }
    ) |>
    purrr$list_rbind()
  df_n_crises = df_all_crises|> dplyr$group_by(iso3, date) |> dplyr$summarise(count = dplyr$n_distinct(crisis_id), .groups = "drop")

  df_n_crises = df_all_crises|> dplyr$group_by(iso3, date) |> dplyr$summarise(count = dplyr$n_distinct(crisis_id), .groups = "drop")

  # # Get risks list
  # df_risks<-httr2$request("https://api.acaps.org/api/v1/risk-list/") |>
  #   httr2$req_url_path_append(
  #     formatted_date
  #   ) |>
  #   httr2$req_headers(
  #     Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
  #   ) |>
  #   httr2$req_retry(
  #     max_tries = 5,
  #     backoff = \(x) x / 10
  #   ) |>
  #   httr2$req_error( # only return an error if not in the latest month
  #     is_error = \(resp) if (format(Sys.Date(), "%b%Y") == formatted_date || !httr2$resp_is_error(resp)) FALSE else TRUE
  #   ) |>
  #   httr2$req_perform() |>
  #   httr2$resp_body_json() |>
  #   purrr$pluck(
  #     "results"
  #   ) |>
  #   purrr$map(
  #     .f = \(x) {
  #       dplyr$tibble(
  #         risk_id = x$risk_id,
  #         risk_title = x$risk_title,
  #         geographical_level = x$geographical_level,
  #         risk_type = x$risk_type,
  #         iso3 = unlist(x$iso3),
  #         country = unlist(x$country),
  #         crisis_id = x$crisis_id,
  #         rationale = x$rationale,
  #         trigger = x$trigger,
  #         vulnerability = x$vulnerability,
  #         date = as.Date(x$`_internal_filter_date`)
  #       )
  #     }
  #   ) |>
  #   purrr$list_rbind()


  df<- df_country_crises|>dplyr$left_join(df_n_crises, by=c('iso3', 'date'))
  df

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
