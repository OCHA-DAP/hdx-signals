box::use(
  httr2,
  dplyr,
  logger,
  stats
)

box::use(
  src / utils / get_env,
  cs = src / utils / cloud_storage
)

# Define the API URL
api_base_url <- "https://acleddata.com/api/acled/read"
api_base_url <- "https://acleddata.com/api/acled/read?_format=json"
#' Download ACLED conflict data
#'
#' This function downloads ACLED conflict data using OAuth authentication.
#' It includes retry logic to handle potential GitHub Actions issues.
#'
#' @return A data frame containing ACLED conflict data
#'
#' @export
raw <- function() {
  # Check if we already downloaded data today
  date_check <- tryCatch(
    {
      cs$read_az_file("output/acled_conflict/download_date.parquet")
    },
    error = function(e) NULL
  )

  if (!is.null(date_check) && date_check$acled_download_date == Sys.Date()) {
    logger$log_debug("ACLED data already downloaded today, loading from cache")
    return(cs$read_az_file("output/acled_conflict/raw.parquet"))
  }

  logger$log_debug("Downloading ACLED data")

  # Get credentials
  username <- get_env$get_env("ACLED_USERNAME")
  password <- get_env$get_env("ACLED_PASSWORD")

  if (is.null(username) || is.null(password) || username == "" || password == "") {
    logger$log_error("ACLED credentials are missing or empty")
    stop("ACLED credentials are missing or empty", call. = FALSE)
  }

  # Log credential format for debugging (without exposing actual values)
  logger$log_debug(paste("Username format check - length:", nchar(username), "contains @:", grepl("@", username)))
  logger$log_debug(paste("Password format check - length:", nchar(password)))

  # OAuth authentication with attempted IP block bypass techniques
  logger$log_debug("Starting OAuth authentication for ACLED with advanced CloudFlare bypass")
  access_token <- NULL
  max_retries <- 5 # Increase retries for CloudFlare

  for (attempt in 1:max_retries) {
    if (attempt > 1) {
      # Much longer delays for CloudFlare - wait 5-15 minutes between attempts
      delay_minutes <- stats$runif(1, 5, 15)
      logger$log_debug(paste("OAuth retry attempt", attempt, "of", max_retries, "- waiting", round(delay_minutes, 1), "minutes"))
      Sys.sleep(delay_minutes * 60)
    } else {
      # Initial delay
      Sys.sleep(stats$runif(1, 1, 3))
    }

    auth_success <- tryCatch(
      {
        resp <- httr2$request("https://acleddata.com/oauth/token") |>
          httr2$req_method("POST") |>
          httr2$req_headers(
            # Use a more realistic browser User-Agent
            "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
            "Content-Type" = "application/x-www-form-urlencoded",
            # Add browser-like headers to bypass CloudFlare
            "Accept" = "application/json, text/plain, */*",
            "Accept-Language" = "en-US,en;q=0.9",
            "Accept-Encoding" = "gzip, deflate, br",
            "Cache-Control" = "no-cache",
            "Pragma" = "no-cache",
            "Sec-Fetch-Dest" = "empty",
            "Sec-Fetch-Mode" = "cors",
            "Sec-Fetch-Site" = "same-origin",
            "X-Requested-With" = "XMLHttpRequest",
            # Additional CloudFlare bypass headers
            "Referer" = "https://acleddata.com/",
            "Origin" = "https://acleddata.com",
            "Connection" = "keep-alive",
            "Upgrade-Insecure-Requests" = "1"
          ) |>
          httr2$req_body_form(
            username = username,
            password = password,
            grant_type = "password",
            client_id = "acled"
          ) |>
          httr2$req_perform()

        logger$log_debug(paste("OAuth response status:", httr2$resp_status(resp)))

        if (httr2$resp_status(resp) == 200) {
          resp_json <- httr2$resp_body_json(resp)
          if (!is.null(resp_json$access_token)) {
            access_token <<- resp_json$access_token
            logger$log_debug("Successfully obtained OAuth token")
            TRUE
          } else {
            logger$log_warn("OAuth response missing access_token")
            FALSE
          }
        } else {
          logger$log_warn(paste("OAuth failed with status:", httr2$resp_status(resp)))
          FALSE
        }
      },
      error = function(e) {
        if (inherits(e, "httr2_http")) {
          logger$log_warn(paste("OAuth HTTP error attempt", attempt, "- Status:", e$status))

          tryCatch(
            {
              response_body <- httr2$resp_body_string(e$resp)
              content_type <- httr2$resp_header(e$resp, "content-type") %||% "unknown"
              logger$log_debug(paste("Response content-type:", content_type))
              logger$log_debug(paste("Response body (first 200 chars):", substr(response_body, 1, 200)))

              if (grepl("text/html", content_type, ignore.case = TRUE)) {
                logger$log_warn("OAuth endpoint returned HTML instead of JSON - possible redirect or IP block")
              }
            },
            error = function(body_err) {
              logger$log_debug("Could not read OAuth response body")
            }
          )
        } else {
          logger$log_warn(paste("OAuth error attempt", attempt, ":", e$message))
        }
        FALSE
      }
    )

    if (auth_success) {
      break
    }

    if (attempt == max_retries) {
      stop("OAuth authentication failed after all retry attempts. This may be due to IP-based blocking in GitHub Actions environment.", call. = FALSE)
    }
  }

  # Now perform data request with the access token
  logger$log_debug("Requesting ACLED data with OAuth token")
  df_acled <- tryCatch(
    {
      # Add delay before data request
      Sys.sleep(stats$runif(1, 1, 3))

      resp <- httr2$request(api_base_url) |>
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
          limit = 0
        ) |>
        httr2$req_auth_bearer_token(access_token) |>
        httr2$req_headers(
          # Use same browser-like User-Agent
          "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
          "Accept" = "application/json, text/plain, */*",
          "Accept-Language" = "en-US,en;q=0.9",
          "Accept-Encoding" = "gzip, deflate, br",
          "Cache-Control" = "no-cache",
          "Pragma" = "no-cache"
        ) |>
        httr2$req_perform()

      logger$log_debug(paste("Data response status:", httr2$resp_status(resp)))

      if (httr2$resp_status(resp) == 200) {
        data <- httr2$resp_body_json(resp, simplifyVector = TRUE)
        logger$log_debug(paste("Successfully retrieved", nrow(data), "ACLED records"))
        data
      } else {
        stop(paste("Data request failed with status:", httr2$resp_status(resp)))
      }
    },
    error = function(e) {
      if (inherits(e, "httr2_http")) {
        logger$log_error(paste("HTTP error requesting ACLED data. Status:", e$status))

        tryCatch(
          {
            response_body <- httr2$resp_body_string(e$resp)
            logger$log_error(paste("ACLED data response body:", substr(response_body, 1, 300)))
          },
          error = function(body_err) {
            logger$log_error("Could not read ACLED data response body")
          }
        )
      } else {
        logger$log_error(paste("Failed to get ACLED data:", e$message))
      }
      stop(paste("Failed to get ACLED data:", e$message), call. = FALSE)
    }
  )

  # Store the download date and data
  dplyr$tibble(
    acled_download_date = Sys.Date()
  ) |>
    cs$update_az_file("output/acled_conflict/download_date.parquet")

  cs$update_az_file(df_acled, "output/acled_conflict/raw.parquet")

  df_acled
}
