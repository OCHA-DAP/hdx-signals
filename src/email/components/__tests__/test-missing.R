test_that("NULL values are missing", {
  expect_true(missing_text(NULL))
})

test_that("NA values are missing", {
  expect_true(missing_text(NA))
})

test_that("Empty string is missing", {
  expect_true(missing_text(""))
})

test_that("Character string is not missing", {
  expect_true(!missing_text("ABC"))
})

test_that("Numeric vector is not missing", {
  expect_true(!missing_text(123))
})
