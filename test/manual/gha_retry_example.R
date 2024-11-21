
# File to store the state of runs
flag_file <- "flag.txt"

# Check if the flag file exists
if (file.exists(flag_file)) {
  message("Success on retry!")
  # Simulate success
} else {
  # Create the flag file to indicate this is the first run
  writeLines("first run", flag_file)
  message("Simulating failure...")
  stop("Intentional failure to test retry mechanism")
}

# box::use(
#   mockery[...]
#   )
#
#
# # Create a mocked function to simulate success on the second call
# simulate_behavior <- mock(
#   function() stop("Simulated failure"),
#   function() message("Simulated success!")
# )
#
# # Use the mocked function
# tryCatch(
#   simulate_behavior(),
#   error = function(e) {
#     message("Retry triggered: ", e$message)
#     # Retry logic (this will be handled by GitHub Actions retry)
#     stop(e)  # Force failure to trigger retry
#   }
# )
#
# # If it succeeds
# simulate_behavior()
#
# # Function that would normally do some real work
# do_real_work <- function() {
#   # In real life, this would do something meaningful
#   return("success")
# }
#
# # Main function that uses do_real_work
# main <- function() {
#   result <- do_real_work()
#   return(result)
# }
#
# # Set up the mock
# do_real_work_mock <- mock()
# stub(main, "do_real_work", do_real_work_mock)
#
# # Configure mock behavior
# do_real_work_mock$
#   # First call throws error
#   throws("Simulated failure")$
#   # Second call returns success
#   returns("Successfully completed on retry")
#
# # Run the function with error handling
# tryCatch({
#   result <- main()
#   cat("Result:", result, "\n")
# }, error = function(e) {
#   message("Error occurred: ", e$message)
#   stop(e)
# })
#
#
# # get_external_data <- function() {
# #   # Get the GitHub run attempt number
# #   run_attempt <- as.numeric(Sys.getenv("GH_RUN_ATTEMPT"))#, unset = "1"))
# #   run_number <- as.numeric(Sys.getenv("GH_RUN_NUMBER", unset = "0"))
# #
# #   # Debugging output
# #   cat("Current run attempt:", run_attempt, "\n")
# #   cat("Current run number:", run_number, "\n")
# #
# #   if (run_attempt == 1) {
# #     # First attempt - always fail
# #     stop("First attempt always fails")
# #   } else {
# #     # Retry attempt - succeed
# #     return("Data retrieved successfully")
# #   }
# # }
# #
# # # Main function with error handling
# # tryCatch({
# #   result <- get_external_data()
# #   cat("Result:", result, "\n")
# # }, error = function(e) {
# #   message("Error: ", e$message)
# #   stop(e)
# # })
