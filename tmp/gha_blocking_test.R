#!/usr/bin/env Rscript

# Minimal test to analyze blocking in GitHub Actions environment
# Run this in GitHub Actions to get the full details

box::use(httr2)

cat("=== GITHUB ACTIONS BLOCKING ANALYSIS ===\n")
cat("Environment: GitHub Actions\n")
cat("Time:", Sys.time(), "\n\n")

# Get basic environment info
cat("=== ENVIRONMENT INFO ===\n")
cat("R version:", R.version.string, "\n")
cat("System:", Sys.info()["sysname"], "\n")
cat("User:", Sys.info()["user"], "\n\n")

# Try OAuth request with minimal headers
username <- Sys.getenv("ACLED_USERNAME")
password <- Sys.getenv("ACLED_PASSWORD")

if (username == "" || password == "") {
  cat("❌ ACLED credentials missing\n")
  quit(status = 1)
}

cat("=== OAUTH REQUEST ATTEMPT ===\n")
tryCatch({
  resp <- httr2$request("https://acleddata.com/oauth/token") |>
    httr2$req_method("POST") |>
    httr2$req_headers("User-Agent" = "R-httr2") |>
    httr2$req_body_form(
      username = username,
      password = password,
      grant_type = "password",
      client_id = "acled"
    ) |>
    httr2$req_perform()
  
  cat("✓ SUCCESS - Status:", httr2$resp_status(resp), "\n")
  
}, error = function(e) {
  if (inherits(e, "httr2_http")) {
    cat("❌ HTTP ERROR - Status:", e$status, "\n\n")
    
    # Get ALL headers
    cat("=== ALL RESPONSE HEADERS ===\n")
    headers <- httr2$resp_headers(e$resp)
    for(name in names(headers)) {
      cat(name, ":", headers[[name]], "\n")
    }
    
    cat("\n=== CLOUDFLARE DETECTION ===\n")
    
    # Check for CloudFlare signatures in headers
    cf_headers <- c("cf-ray", "cf-cache-status", "cf-request-id", "server")
    for(header in cf_headers) {
      if (header %in% names(headers)) {
        cat("🔴 CloudFlare header found -", header, ":", headers[[header]], "\n")
      }
    }
    
    # Check server header specifically
    server <- headers[["server"]] %||% ""
    if (grepl("cloudflare", server, ignore.case = TRUE)) {
      cat("🔴 CloudFlare server confirmed\n")
    }
    
    # Get response body
    cat("\n=== RESPONSE BODY ===\n")
    tryCatch({
      body <- httr2$resp_body_string(e$resp)
      cat("Body length:", nchar(body), "\n")
      
      # CloudFlare specific checks
      if (grepl("Just a moment", body)) {
        cat("🔴 CONFIRMED: CloudFlare 'Just a moment' page\n")
      }
      if (grepl("cloudflare", body, ignore.case = TRUE)) {
        cat("🔴 CONFIRMED: CloudFlare mentioned in body\n")
      }
      if (grepl("ray.*id", body, ignore.case = TRUE)) {
        cat("🔴 CONFIRMED: CloudFlare Ray ID pattern in body\n")
      }
      
      cat("\nFirst 300 characters:\n")
      cat(substr(body, 1, 300), "\n")
      
    }, error = function(body_err) {
      cat("❌ Could not read body:", body_err$message, "\n")
    })
    
  } else {
    cat("❌ Non-HTTP error:", e$message, "\n")
  }
})
