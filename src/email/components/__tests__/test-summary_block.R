test_that("Missing values still return character", {
  expect_s3_class(add_summary(NA, NA), "glue")
})

test_that("Character values passed through", {
  expect_s3_class(add_summary("ABC", "XYZ"), "glue")
})
