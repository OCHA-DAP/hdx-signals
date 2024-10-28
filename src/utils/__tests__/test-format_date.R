test_that("format_date works correctly", {
  expect_equal(
    format_date(as.Date("2023-12-31")), "31 December 2023"
  )
  expect_equal(
    format_date(as.Date("1978-01-04")), "4 January 1978"
  )
})
