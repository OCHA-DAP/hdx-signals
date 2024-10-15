test_that("Missing values converted to empty string", {
  expect_equal(add_further_info(NA), "")
})

test_that("Character values passed through", {
  expect_type(add_further_info("Test"), "character")
})
