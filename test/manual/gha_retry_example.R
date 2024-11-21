get_external_data <- function() {
  # Get the GitHub run attempt number
  run_attempt <- as.numeric(Sys.getenv("GH_RUN_ATTEMPT"))#, unset = "1"))
  run_number <- as.numeric(Sys.getenv("GH_RUN_NUMBER", unset = "0"))

  # Debugging output
  cat("Current run attempt:", run_attempt, "\n")
  cat("Current run number:", run_number, "\n")

  if (run_attempt == 1) {
    # First attempt - always fail
    stop("First attempt always fails")
  } else {
    # Retry attempt - succeed
    return("Data retrieved successfully")
  }
}

# Main function with error handling
tryCatch({
  result <- get_external_data()
  cat("Result:", result, "\n")
}, error = function(e) {
  message("Error: ", e$message)
  stop(e)
})
