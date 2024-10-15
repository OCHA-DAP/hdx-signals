test_that("Runs and returns single string", {
  template <- create_template(
    body = "A",
    banner_url = "B"
  )

  expect_s3_class(template, "glue")
  expect_length(template, 1)
})
