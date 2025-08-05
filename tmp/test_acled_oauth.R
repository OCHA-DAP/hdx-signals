box::use(
    httr2,
    logger,
    dplyr
)

# Set up logging
logger$log_threshold(logger$DEBUG)
logger$log_appender(logger$appender_console)
logger$log_formatter(logger$formatter_sprintf)

#' Test ACLED OAuth authentication following exact ACLED documentation
#'
#' This function tests OAuth authentication using the exact method specified
#' in ACLED's API documentation, with comprehensive logging for debugging.
#'
#' @export
test_acled_oauth <- function() {
    logger$log_info("=== ACLED OAuth Authentication Test ===")
    logger$log_info("Environment: %s", ifelse(Sys.getenv("GITHUB_ACTIONS") == "true", "GitHub Actions", "Local"))
    logger$log_info("R version: %s", R.version.string)
    logger$log_info("Time: %s", Sys.time())

    # Get IP address information for ACLED support
    logger$log_info("=== IP ADDRESS INFORMATION FOR ACLED SUPPORT ===")
    tryCatch(
        {
            # Try to get public IP using httr2 (same method our OAuth will use)
            ip_response <- httr2$request("https://api.ipify.org") |>
                httr2$req_perform()
            if (httr2$resp_status(ip_response) == 200) {
                public_ip <- httr2$resp_body_string(ip_response)
                logger$log_info("🌐 Public IP Address: %s", public_ip)
            } else {
                logger$log_warn("Could not determine public IP address")
            }
        },
        error = function(e) {
            logger$log_warn("Failed to get IP address: %s", e$message)
        }
    )

    # Try alternative IP service
    tryCatch(
        {
            ip_response2 <- httr2$request("https://httpbin.org/ip") |>
                httr2$req_perform()
            if (httr2$resp_status(ip_response2) == 200) {
                ip_data <- httr2$resp_body_json(ip_response2)
                logger$log_info("🌐 Public IP (httpbin): %s", ip_data$origin)
            }
        },
        error = function(e) {
            logger$log_debug("Alternative IP service failed: %s", e$message)
        }
    )
    logger$log_info("================================================")

    # Get credentials
    username <- Sys.getenv("ACLED_USERNAME")
    password <- Sys.getenv("ACLED_PASSWORD")

    if (username == "" || password == "") {
        logger$log_error("❌ ACLED credentials not found in environment variables")
        stop("ACLED credentials missing", call. = FALSE)
    }

    logger$log_debug("Credentials check - Username: %d chars, Password: %d chars", nchar(username), nchar(password))

    # Step 1: Get OAuth access token (following ACLED documentation exactly)
    logger$log_info("Step 1: Requesting OAuth access token")
    token_url <- "https://acleddata.com/oauth/token"
    logger$log_debug("OAuth endpoint: %s", token_url)

    token_response <- tryCatch(
        {
            httr2$request(token_url) |>
                httr2$req_method("POST") |>
                httr2$req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
                httr2$req_body_form(
                    username = username,
                    password = password,
                    grant_type = "password",
                    client_id = "acled"
                ) |>
                httr2$req_perform()
        },
        error = function(e) {
            if (inherits(e, "httr2_http")) {
                logger$log_error("❌ OAuth HTTP error - Status: %d", e$status)

                # Try to get response details for debugging
                tryCatch(
                    {
                        response_body <- httr2$resp_body_string(e$resp)
                        content_type <- httr2$resp_header(e$resp, "content-type") %||% "unknown"
                        logger$log_error("Response content-type: %s", content_type)
                        logger$log_error("Response body (first 300 chars): %s", substr(response_body, 1, 300))

                        # Check for CloudFlare blocking
                        if (grepl("text/html", content_type, ignore.case = TRUE)) {
                            logger$log_error("⚠️  OAuth endpoint returned HTML instead of JSON")
                            if (grepl("Just a moment", response_body, ignore.case = TRUE)) {
                                logger$log_error("🔴 CONFIRMED: CloudFlare 'Just a moment' challenge page detected")
                            }
                            if (grepl("cloudflare", response_body, ignore.case = TRUE)) {
                                logger$log_error("🔴 CONFIRMED: CloudFlare blocking detected in response body")
                            }
                        }

                        # Check response headers for CloudFlare
                        headers <- httr2$resp_headers(e$resp)
                        if (!is.null(headers$server) && grepl("cloudflare", headers$server, ignore.case = TRUE)) {
                            logger$log_error("🔴 CONFIRMED: CloudFlare server header detected")
                        }
                        if (!is.null(headers$`cf-ray`)) {
                            logger$log_error("🔴 CONFIRMED: CloudFlare Ray ID: %s", headers$`cf-ray`)
                        }
                    },
                    error = function(body_err) {
                        logger$log_debug("Could not read OAuth response details: %s", body_err$message)
                    }
                )
            } else {
                logger$log_error("❌ OAuth error: %s", e$message)
            }
            stop("OAuth authentication failed", call. = FALSE)
        }
    )

    # Check OAuth response
    oauth_status <- httr2$resp_status(token_response)
    logger$log_debug("OAuth response status: %d", oauth_status)

    if (oauth_status == 200) {
        token_data <- httr2$resp_body_json(token_response)
        access_token <- token_data$access_token
        logger$log_info("✅ Successfully obtained OAuth access token")
        logger$log_debug("Token type: %s", token_data$token_type %||% "unknown")
        logger$log_debug("Token expires in: %d seconds", token_data$expires_in %||% "unknown")
    } else {
        logger$log_error("❌ OAuth authentication failed with status: %d", oauth_status)
        stop("OAuth authentication failed", call. = FALSE)
    }

    # Step 2: Use access token to request data
    logger$log_info("Step 2: Requesting ACLED data with Bearer token")
    api_url <- "https://acleddata.com/api/acled/read?_format=json&country=Georgia&year=2023&fields=event_id_cnty|event_date|event_type|country|fatalities&limit=10"
    logger$log_debug("API URL: %s", api_url)

    data_response <- tryCatch(
        {
            httr2$request(api_url) |>
                httr2$req_headers(
                    "Authorization" = paste("Bearer", access_token),
                    "Content-Type" = "application/json"
                ) |>
                httr2$req_perform()
        },
        error = function(e) {
            if (inherits(e, "httr2_http")) {
                logger$log_error("❌ Data request HTTP error - Status: %d", e$status)
                tryCatch(
                    {
                        error_body <- httr2$resp_body_string(e$resp)
                        logger$log_error("Data request error response: %s", substr(error_body, 1, 200))
                    },
                    error = function(body_err) {
                        logger$log_debug("Could not read data request error response")
                    }
                )
            } else {
                logger$log_error("❌ Data request error: %s", e$message)
            }
            stop("Data request failed", call. = FALSE)
        }
    )

    # Check data response
    data_status <- httr2$resp_status(data_response)
    logger$log_debug("Data request status: %d", data_status)

    if (data_status == 200) {
        response_data <- httr2$resp_body_json(data_response, simplifyVector = TRUE)

        if (!is.null(response_data$data) && nrow(response_data$data) > 0) {
            acled_data <- response_data$data
            logger$log_info("✅ Successfully retrieved %d ACLED records", nrow(acled_data))
            logger$log_info("Date range: %s to %s", min(acled_data$event_date), max(acled_data$event_date))
            logger$log_info("Countries: %s", paste(unique(acled_data$country), collapse = ", "))
            logger$log_info("Event types: %s", paste(unique(acled_data$event_type), collapse = ", "))

            # Show sample records
            logger$log_debug("Sample records:")
            for (i in 1:min(3, nrow(acled_data))) {
                logger$log_debug("  Record %d: %s - %s (%s)", i, acled_data$event_date[i], acled_data$event_type[i], acled_data$country[i])
            }

            return(acled_data)
        } else {
            logger$log_warn("⚠️  Data request successful but no records returned")
            return(data.frame())
        }
    } else {
        logger$log_error("❌ Data request failed with status: %d", data_status)
        stop("Data request failed", call. = FALSE)
    }
}

# Run the test
logger$log_info("Starting ACLED OAuth authentication test...")
result <- test_acled_oauth()
logger$log_info("=== Test completed successfully ===")
logger$log_info("Total records retrieved: %d", nrow(result))
