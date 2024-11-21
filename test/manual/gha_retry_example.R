
# Function to check retry state
get_external_data <- function() {
  # Use GITHUB_RUN_ATTEMPT environment variable
  # This is a built-in GitHub Actions variable that increments on retries
  retry_attempt <- as.numeric(Sys.getenv("GITHUB_RUN_ATTEMPT", unset = "1"))

  # Debugging output
  cat("Current attempt:", retry_attempt, "\n")

  if (retry_attempt == 1) {
    # First attempt - always fail
    stop("First attempt always fails")
  } else if (retry_attempt == 2) {
    # First retry - succeed
    return("Data retrieved successfully")
  }

  # Any subsequent attempts fail
  stop("Unexpected retry attempt")
}

# Main function with error handling
tryCatch({
  result <- get_external_data()
  cat("Result:", result, "\n")
}, error = function(e) {
  message("Error: ", e$message)
  stop(e)
})
