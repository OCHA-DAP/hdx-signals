box::use(
  httr2,
  dplyr,
  purrr,
  readr,
  logger,
  jsonlite
)

box::use(
  src / utils / get_env,
  cs = src / utils / cloud_storage
)

# Define the API and token URLs
api_base_url <- "https://acleddata.com/api/acled/read?_format=json"
token_url <- "https://acleddata.com/oauth/token"
# Get credentials from environment variables
username <- get_env$get_env("ACLED_USERNAME")
password <- get_env$get_env("ACLED_PASSWORD")


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

    # Validate credentials are available
    if (is.null(username) || is.null(password) || username == "" || password == "") {
      logger$log_error("ACLED credentials are missing or empty")
      stop("ACLED credentials are missing or empty", call. = FALSE)
    }

    # Log credential format for debugging (without exposing actual values)
    logger$log_debug(paste("Username format check - length:", nchar(username), "contains @:", grepl("@", username)))
    logger$log_debug(paste("Password format check - length:", nchar(password)))

    # First, get OAuth token manually to match ACLED's specific format requirements
    logger$log_debug("Requesting OAuth token from ACLED")
    token_response <- tryCatch(
      {
        # Try form-urlencoded format as shown in ACLED's Python example
        resp <- httr2$request(token_url) |>
          httr2$req_method("POST") |>
          httr2$req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
          httr2$req_body_form(
            username = username,
            password = password,
            grant_type = "password",
            client_id = "acled"
          ) |>
          httr2$req_perform()

        # Log response details for debugging
        logger$log_debug(paste("Token response status:", httr2$resp_status(resp)))
        logger$log_debug(paste("Token response headers:", paste(names(httr2$resp_headers(resp)), collapse = ", ")))

        httr2$resp_body_json(resp)
      },
      error = function(e) {
        # Try to capture more details about the error
        if (inherits(e, "httr2_http")) {
          logger$log_error(paste("HTTP error getting OAuth token. Status:", e$status))
          logger$log_error(paste("Response body:", httr2$resp_body_string(e$resp)))
        } else {
          logger$log_error(paste("Failed to get OAuth token:", e$message))
        }
        stop(paste("Failed to get OAuth token:", e$message), call. = FALSE)
      }
    )

    if (is.null(token_response$access_token)) {
      logger$log_error("OAuth token response did not contain access_token")
      logger$log_debug(paste("Token response:", jsonlite$toJSON(token_response, auto_unbox = TRUE)))
      stop("OAuth token response did not contain access_token", call. = FALSE)
    }

    access_token <- token_response$access_token
    logger$log_debug("Successfully obtained OAuth token")

    # Now perform request with the access token
    logger$log_debug("Requesting ACLED data with OAuth token")
    df_acled <- tryCatch(
      {
        httr2$request(
          api_base_url
        ) |>
          httr2$req_url_query(
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
          httr2$req_headers("Authorization" = paste("Bearer", access_token)) |>
          httr2$req_perform() |>
          httr2$resp_body_json() |>
          purrr$pluck("data") |>
          purrr$map(dplyr$as_tibble) |>
          purrr$list_rbind() |>
          readr$type_convert(
            col_types = readr$cols()
          )
      },
      error = function(e) {
        logger$log_error(paste("Failed to get ACLED data:", e$message))
        stop(paste("Failed to get ACLED data:", e$message), call. = FALSE)
      }
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
