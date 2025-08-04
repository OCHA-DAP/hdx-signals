library(httr2) # For handling API authorization and requests
library(jsonlite) # For handling the response of the API
library(dplyr) # For handling data
library(logger) # For logging

# Set up logging
log_threshold(DEBUG)
log_appender(appender_console)
log_formatter(formatter_sprintf)

# Function to get access token using username and password (following ACLED documentation)
get_access_token <- function(username, password) {
  token_url <- "https://acleddata.com/oauth/token"

  log_info("Starting OAuth authentication with ACLED API")
  log_debug("OAuth endpoint: %s", token_url)
  log_debug("Username length: %d characters", nchar(username))

  response <- httr2::request(token_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
    httr2::req_body_form(
      username = username,
      password = password,
      grant_type = "password",
      client_id = "acled"
    ) |>
    httr2::req_perform()

  status_code <- httr2::resp_status(response)
  log_debug("OAuth response status: %d", status_code)

  if (status_code == 200) {
    token_data <- httr2::resp_body_json(response)
    log_info("✅ Successfully obtained OAuth access token")
    log_debug("Token expires in: %d seconds", token_data$expires_in %||% "unknown")
    return(token_data$access_token)
  } else {
    log_error("❌ OAuth authentication failed with status: %d", status_code)

    # Try to get response details for debugging
    tryCatch(
      {
        response_body <- httr2::resp_body_string(response)
        content_type <- httr2::resp_header(response, "content-type") %||% "unknown"
        log_error("Response content-type: %s", content_type)
        log_error("Response body (first 200 chars): %s", substr(response_body, 1, 200))

        if (grepl("text/html", content_type, ignore.case = TRUE)) {
          log_error("⚠️  OAuth endpoint returned HTML instead of JSON - possible CloudFlare blocking")
        }
      },
      error = function(e) {
        log_debug("Could not read OAuth response body: %s", e$message)
      }
    )

    stop(paste("Failed to get access token:", status_code))
  }
}

# Get access token
log_info("=== ACLED API Authentication Test ===")
log_info("Environment: %s", ifelse(Sys.getenv("GITHUB_ACTIONS") == "true", "GitHub Actions", "Local"))
log_info("R version: %s", R.version.string)

username <- Sys.getenv("ACLED_USERNAME")
password <- Sys.getenv("ACLED_PASSWORD")

if (username == "" || password == "") {
  log_error("❌ ACLED credentials not found in environment variables")
  stop("ACLED credentials missing")
}

log_debug("Credentials check - Username: %d chars, Password: %d chars", nchar(username), nchar(password))

my_token <- get_access_token(
  username = username,
  password = password
)

# Option #1 (parameters in the url)
log_info("Requesting ACLED data with Bearer token authentication")
api_url <- "https://acleddata.com/api/acled/read?_format=json&country=Georgia:OR:country=Armenia&year=2021&fields=event_id_cnty|event_date|event_type|country|fatalities"
log_debug("API URL: %s", api_url)

response <- httr2::request(api_url) |>
  httr2::req_headers(
    "Authorization" = paste("Bearer", my_token),
    "Content-Type" = "application/json"
  ) |>
  httr2::req_perform()

data_status <- httr2::resp_status(response)
log_debug("Data request status: %d", data_status)

if (data_status == 200) {
  response_df_option1 <- httr2::resp_body_json(response, simplifyVector = TRUE)$data
  log_info("✅ Successfully retrieved %d ACLED records", nrow(response_df_option1))
  log_info("Date range: %s to %s", min(response_df_option1$event_date), max(response_df_option1$event_date))
  log_info("Countries: %s", paste(unique(response_df_option1$country), collapse = ", "))
} else {
  log_error("❌ Data request failed with status: %d", data_status)
  tryCatch(
    {
      error_body <- httr2::resp_body_string(response)
      log_error("Error response: %s", substr(error_body, 1, 200))
    },
    error = function(e) {
      log_debug("Could not read error response body")
    }
  )
  stop("Data request failed")
}
#
#
# # Option #2 (parameters as a list)
#
# # Set up the list of parameters
# parameters <- list(
#   country = "Georgia:OR:country=Armenia",
#   year = 2021,
#   fields = "event_id_cnty|event_date|event_type|country|fatalities"
# )
#
# response <- httr2::request("https://acleddata.com/api/acled/read?_format=json") |>
#   httr2::req_headers(
#     "Authorization" = paste("Bearer", my_token),
#     "Content-Type" = "application/json"
#   ) |>
#   httr2::req_url_query(!!!parameters) |>
#   httr2::req_perform()
#
# response_df_option2 <- httr2::resp_body_json(response, simplifyVector = TRUE)$data
