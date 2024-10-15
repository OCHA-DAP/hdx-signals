test_that("Returns length one character", {
  line_text <- add_line()

  expect_s3_class(line_text, "glue")
  expect_length(line_text, 1)
})
