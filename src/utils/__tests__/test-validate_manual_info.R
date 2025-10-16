box::use(src/utils/validate_manual_info[validate_manual_info, validate_dates, validate_ipc_food_insecurity])

impl <- attr(validate_manual_info, "namespace")

# Test 1
test_that("validate_manual_info raise error for missing required columns", {
  # DataFrame without date column
  df_missing_date <- data.frame(
    iso3 = "AFG",
    indicator_id = "wfp_market_monitor",
    text1 = "Some text"
  )

  expect_error(
    validate_manual_info(df_missing_date),
    "Missing required columns: date"
  )

  # DataFrame without column text1 (required)
  df_missing_text1 <- data.frame(
    iso3 = "AFG",
    indicator_id = "wfp_market_monitor",
    date = "2024-01-15"
  )

  expect_error(
    validate_manual_info(df_missing_text1),
    "Missing required columns: text1"
  )

  # DataFrame without multiple required columns
  df_missing_multiple <- data.frame(
    indicator_id = "wfp_market_monitor"
  )

  expect_error(
    validate_manual_info(df_missing_multiple),
    "Missing required columns"
  )
})

# Test 2
test_that("validate_manual_info filters invalid iso3 codes", {
  # Mock all_iso3_code list
  mock_iso3_codes <- mock(c("AFG", "ITA", "USA"))
  stub(validate_manual_info, "all_iso3_codes$all_iso3_codes", mock_iso3_codes)

  df_test <- data.frame(
    iso3 = c("AFG", "XXX", "ITA", "", NA, "USA"),
    indicator_id = rep("wfp_market_monitor", 6),
    date = rep("2024-01-15", 6),
    text1 = rep("Valid text", 6),
    stringsAsFactors = FALSE
  )

  result <- validate_manual_info(df_test)

  expect_called(mock_iso3_codes, 1)

  expect_equal(nrow(result), 3)
  expect_equal(result$iso3, c("AFG", "ITA", "USA"))
})

# Test 3: Invalid indicator_id values
test_that("validate_manual_info filters invalid indicator_id values", {
  mock_iso3_codes <- mock(c("AFG"))
  stub(validate_manual_info, "all_iso3_codes$all_iso3_codes", mock_iso3_codes)

  df_test <- data.frame(
    iso3 = rep("AFG", 5),
    indicator_id = c(
      "wfp_market_monitor", # should keep row 1
      "invalid_indicator",
      "acled_conflict",     # should keep row 3
      "",
      NA
    ),
    date = rep("2024-01-15", 5),
    text1 = rep("Valid text", 5),
    stringsAsFactors = FALSE
  )

  result <- validate_manual_info(df_test)

  expect_equal(nrow(result), 2)
  expect_equal(result$indicator_id, c("wfp_market_monitor", "acled_conflict"))
})

# Test 4: Check that function filters dates with invalid format
test_that("validate_manual_info filters dates with invalid format", {
  mock_iso3_codes <- mock(c("AFG"))
  stub(validate_manual_info, "all_iso3_codes$all_iso3_codes", mock_iso3_codes)

  df_test <- data.frame(
    iso3 = rep("AFG", 7),
    indicator_id = rep("wfp_market_monitor", 7),
    date = c(
      "2024-01-15",      # valid
      "2024-13-01",      # invalid month
      "15-01-2024",      # wrong format
      "2024/01/15",      # wrong separator
      "",                # empty
      NA,                # NA
      "2024-02-30"       # impossible date
    ),
    text1 = rep("Valid text", 7),
    stringsAsFactors = FALSE
  )

  result <- validate_manual_info(df_test)

  # Should keep only the first row
  expect_equal(nrow(result), 1)
  expect_equal(result$date, "2024-01-15")
})

# Test 5
test_that("validate_manual_info filters rows with invalid text1", {
  mock_iso3_codes <- mock(c("AFG"))
  stub(validate_manual_info, "all_iso3_codes$all_iso3_codes", mock_iso3_codes)

  df_test <- data.frame(
    iso3 = rep("AFG", 4),
    indicator_id = rep("wfp_market_monitor", 4),
    date = rep("2024-01-15", 4),
    text1 = c("Valid text", "", NA, "Another valid text"),
    stringsAsFactors = FALSE
  )

  result <- validate_manual_info(df_test)

  # Should keep only rows 1 and 4
  expect_equal(nrow(result), 2)
  expect_equal(result$text1, c("Valid text", "Another valid text"))
})

# Test 6
test_that("validate_manual_info keeps completely valid rows", {
  mock_iso3_codes <- mock(c("AFG", "ITA", "SOM"))
  stub(validate_manual_info, "all_iso3_codes$all_iso3_codes", mock_iso3_codes)

  df_test <- data.frame(
    iso3 = c("AFG", "ITA", "SOM"),
    indicator_id = c(
      "wfp_market_monitor",
      "acled_conflict",
      "ipc_food_insecurity"
    ),
    date = c("2024-01-15", "2023-12-01", "2024-03-20"),
    text1 = c("Market update", "Conflict report", "Food insecurity data"),
    value = c(100, 200, 300),  # extra column
    stringsAsFactors = FALSE
  )

  result <- validate_manual_info(df_test)

  # All rows should pass
  expect_equal(nrow(result), 3)
  expect_equal(result$iso3, c("AFG", "ITA", "SOM"))

  # Check that extra columns are preserved
  expect_true("value" %in% names(result))
  expect_equal(result$value, c(100, 200, 300))
})

# Test 7
test_that("validate_manual_info correctly filters invalid indicator_id", {
  mock_iso3_codes <- mock(c("AFG", "ITA"))
  stub(validate_manual_info, "all_iso3_codes$all_iso3_codes", mock_iso3_codes)

  df_test <- data.frame(
    iso3 = c("AFG", "XXX", "ITA", "AFG", "ITA"),
    indicator_id = c(
      "wfp_market_monitor",
      "wfp_market_monitor",
      "invalid_indicator",
      "acled_conflict",
      "who_cholera"
    ),
    date = c(
      "2024-01-15",
      "2024-01-15",
      "2024-01-15",
      "invalid-date",
      "2024-02-20"
    ),
    text1 = c("Valid", "Valid", "Valid", "Valid", "Valid"),
    stringsAsFactors = FALSE
  )

  result <- validate_manual_info(df_test)

  # Should pass only rows 1 and 5
  expect_equal(nrow(result), 2)
  expect_equal(result$iso3, c("AFG", "ITA"))
  expect_equal(result$indicator_id, c("wfp_market_monitor", "who_cholera"))
})

# Test 8
test_that("validate_ipc_food_insecurity copies text1 to text2 when text2 is empty", {
  df_test <- data.frame(
    iso3 = rep("AFG", 4),
    indicator_id = rep("ipc_food_insecurity", 4),
    date = rep("2024-01-15", 4),
    text1 = c("Situation A", "Situation B", "Situation C", "Situation D"),
    text2 = c("", NA, "Existing recommendation", "Another recommendation"),
    stringsAsFactors = FALSE
  )

  result <- validate_ipc_food_insecurity(df_test)

  # Rows 1 and 2 should have text2 copied from text1
  expect_equal(result$text2[1], "Situation A")
  expect_equal(result$text2[2], "Situation B")
  # Rows 3 and 4 should keep their original text2
  expect_equal(result$text2[3], "Existing recommendation")
  expect_equal(result$text2[4], "Another recommendation")
})

# Test 9
test_that("validate_ipc_food_insecurity doesn't modify non-IPC rows", {
  df_test <- data.frame(
    iso3 = rep("AFG", 3),
    indicator_id = c("wfp_market_monitor", "ipc_food_insecurity", "acled_conflict"),
    date = rep("2024-01-15", 3),
    text1 = c("Text A", "Text B", "Text C"),
    text2 = c("", "", ""),
    stringsAsFactors = FALSE
  )

  result <- validate_ipc_food_insecurity(df_test)

  # Only the IPC row (2) should have text2 modified
  expect_equal(result$text2[1], "")
  expect_equal(result$text2[2], "Text B")
  expect_equal(result$text2[3], "")
})

# Test 10
test_that("validate_ipc_food_insecurity handles DataFrame with no IPC rows", {
  df_test <- data.frame(
    iso3 = rep("AFG", 2),
    indicator_id = c("wfp_market_monitor", "acled_conflict"),
    date = rep("2024-01-15", 2),
    text1 = c("Text A", "Text B"),
    text2 = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- validate_ipc_food_insecurity(df_test)

  # Unchanged
  expect_equal(result, df_test)
})
