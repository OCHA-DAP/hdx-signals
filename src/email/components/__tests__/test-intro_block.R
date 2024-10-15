test_that("Runs and returns single string", {
  intro <- add_intro(
    title = "TITLE",
    iso3 = "ABC",
    location = "XYZ",
    summary_short = "Summary test"
  )

  expect_s3_class(intro, "glue")
  expect_length(intro, 1)
})
