test_that("Missing values converted to empty string", {
  expect_equal(add_image(NA), "")
})

test_that("Character values passed through", {
  expect_type(add_image("Test"), "character")
})
