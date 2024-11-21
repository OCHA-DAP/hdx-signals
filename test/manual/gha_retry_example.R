#' dummy script to test out the functionality of
#' `nick-fields/retry@v3` action


# dummy file - not present on first
dummy_file <- tempfile(fileext = ".txt")

# Check if the flag file exists
if (file.exists(flag_file)) {
  message("Success on retry!") # this should happen on try 2
  # Simulate success
} else {
  # Create the flag file to indicate this is the first run
  writeLines("first run", dummy_file)
  message("Simulating failure...")
  stop("Intentional failure to test retry mechanism")
}
