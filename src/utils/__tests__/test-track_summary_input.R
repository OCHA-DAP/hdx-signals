box::use(
  testthat[...],
  arrow[read_parquet, write_parquet],
  dplyr[tibble, filter]
)

box::use(
  src/signals/track_summary_input
)

# Helper to clean up test files
cleanup_test_files <- function() {
  test_files <- c(
    "output/summary_input.parquet",
    "output/summary_input_dry_run.parquet"
  )
  for (f in test_files) {
    if (file.exists(f)) unlink(f)
  }
}

# Setup and teardown
setup({
  Sys.setenv(HS_LOCAL = "TRUE")
  Sys.setenv(HS_DRY_RUN = "FALSE")
  cleanup_test_files()
})

teardown({
  cleanup_test_files()
})

test_that("append_tracking_data creates new file with correct schema", {
  cleanup_test_files()
  # Create test data
  test_data <- tibble(
    location_iso3 = "AFG",
    date_generated = as.Date("2024-01-01"),
    indicator_id = "test_indicator",
    info = "Test scraped data",
    manual_info = NA_character_,
    use_manual_info = FALSE,
    summary_long = "This is a long summary",
    summary_short = "Short summary",
    summary_source = "Test source"
  )

  # Execute
  track_summary_input$append_tracking_data(test_data)

  # Verify file exists
  path <- "output/summary_input.parquet"
  expect_true(file.exists(path))

  # Read and verify content
  result <- read_parquet(path)

  expect_equal(nrow(result), 1)
  expect_equal(result$location_iso3, "AFG")
  expect_equal(result$indicator_id, "test_indicator")
  expect_equal(result$info, "Test scraped data")
  expect_equal(result$campaign_status, "pending")
  expect_true(is.na(result$triaged_at))
})

test_that("append_tracking_data appends to existing file", {
  cleanup_test_files()

  # First record
  data1 <- tibble(
    location_iso3 = "SOM",
    date_generated = as.Date("2024-01-01"),
    indicator_id = "test_indicator",
    info = "First record",
    manual_info = NA_character_,
    use_manual_info = FALSE,
    summary_long = "Summary 1",
    summary_short = "Short 1",
    summary_source = "Source 1"
  )

  track_summary_input$append_tracking_data(data1)

  # Second record
  data2 <- tibble(
    location_iso3 = "YEM",
    date_generated = as.Date("2024-01-02"),
    indicator_id = "test_indicator",
    info = "Second record",
    manual_info = "Manual info here",
    use_manual_info = TRUE,
    summary_long = "Summary 2",
    summary_short = "Short 2",
    summary_source = "Source 2"
  )

  track_summary_input$append_tracking_data(data2)

  # Verify
  result <- track_summary_input$read_tracking_file()

  expect_equal(nrow(result), 2)
  expect_true("SOM" %in% result$location_iso3)
  expect_true("YEM" %in% result$location_iso3)
  expect_equal(result$campaign_status, c("pending", "pending"))
})

test_that("read_tracking_file returns NULL when file doesn't exist", {
  cleanup_test_files()

  # Execute
  result <- track_summary_input$read_tracking_file()

  # Verify
  expect_null(result)
})

test_that("append_tracking_data validates required columns", {
  cleanup_test_files()

  # Missing required column
  bad_data <- tibble(
    location_iso3 = "AFG",
    date_generated = as.Date("2024-01-01")
    # Missing other required columns
  )

  # Expect error
  expect_error(
    track_summary_input$append_tracking_data(bad_data),
    "Missing required columns"
  )
})

test_that("HS_DRY_RUN creates separate file", {
  Sys.setenv(HS_DRY_RUN = "TRUE")

  test_data <- tibble(
    location_iso3 = "AFG",
    date_generated = as.Date("2024-01-01"),
    indicator_id = "test_indicator",
    info = "Dry run data",
    manual_info = NA_character_,
    use_manual_info = FALSE,
    summary_long = "Summary",
    summary_short = "Short",
    summary_source = "Source"
  )

  track_summary_input$append_tracking_data(test_data)

  # Check dry run file exists
  dry_run_path <- "output/summary_input_dry_run.parquet"
  expect_true(file.exists(dry_run_path))

  # Check normal file doesn't exist
  normal_path <- "output/summary_input.parquet"
  expect_false(file.exists(normal_path))

  Sys.setenv(HS_DRY_RUN = "FALSE")
  cleanup_test_files()
})

test_that("update_tracking_status updates matching signals", {
  cleanup_test_files()

  # Create initial tracking data
  initial_data <- tibble(
    location_iso3 = c("AFG", "SOM", "YEM"),
    date_generated = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    indicator_id = "test_indicator",
    info = "Test info",
    manual_info = NA_character_,
    use_manual_info = FALSE,
    summary_long = "Long",
    summary_short = "Short",
    summary_source = "Source"
  )

  track_summary_input$append_tracking_data(initial_data)

  # Create signals dataframe (simulating triage)
  signals <- tibble(
    iso3 = c("AFG", "SOM"),
    date = as.Date(c("2024-01-01", "2024-01-02")),
    indicator_id = "test_indicator"
  )

  # Update status to "sent"
  track_summary_input$update_tracking_status(signals, "sent")

  # Verify
  result <- track_summary_input$read_tracking_file()

  expect_equal(result$campaign_status, c("sent", "sent", "pending"))
  expect_false(is.na(result$triaged_at[1]))
  expect_false(is.na(result$triaged_at[2]))
  expect_true(is.na(result$triaged_at[3]))
})

test_that("update_tracking_status handles deleted status", {
  cleanup_test_files()

  # Create initial data
  initial_data <- tibble(
    location_iso3 = "AFG",
    date_generated = as.Date("2024-01-01"),
    indicator_id = "test_indicator",
    info = "Test",
    manual_info = NA_character_,
    use_manual_info = FALSE,
    summary_long = "Long",
    summary_short = "Short",
    summary_source = "Source"
  )

  track_summary_input$append_tracking_data(initial_data)

  # Delete signal
  signals <- tibble(
    iso3 = "AFG",
    date = as.Date("2024-01-01"),
    indicator_id = "test_indicator"
  )

  track_summary_input$update_tracking_status(signals, "deleted")

  # Verify
  result <- track_summary_input$read_tracking_file()

  expect_equal(result$campaign_status, "deleted")
  expect_false(is.na(result$triaged_at))
})

test_that("update_tracking_status handles archived status", {
  cleanup_test_files()

  # Create initial data
  initial_data <- tibble(
    location_iso3 = "YEM",
    date_generated = as.Date("2024-01-01"),
    indicator_id = "test_indicator",
    info = "Test",
    manual_info = "Manual",
    use_manual_info = TRUE,
    summary_long = "Long",
    summary_short = "Short",
    summary_source = "Source"
  )

  track_summary_input$append_tracking_data(initial_data)

  # Archive signal
  signals <- tibble(
    iso3 = "YEM",
    date = as.Date("2024-01-01"),
    indicator_id = "test_indicator"
  )

  track_summary_input$update_tracking_status(signals, "archived")

  # Verify
  result <- track_summary_input$read_tracking_file()

  expect_equal(result$campaign_status, "archived")
  expect_false(is.na(result$triaged_at))
})

test_that("update_tracking_status skips if no tracking file exists", {
  cleanup_test_files()

  # Create signals without tracking file
  signals <- tibble(
    iso3 = "AFG",
    date = as.Date("2024-01-01"),
    indicator_id = "nonexistent_indicator"
  )

  # Should not error
  expect_silent(
    track_summary_input$update_tracking_status(signals, "sent")
  )
})
