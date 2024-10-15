test_that("All missing values return empty string", {
  expect_equal(
    add_text(
      text = "",
      header = "",
      pre_header_text = ""
    ),
    ""
  )
})

test_that("Header included if not missing", {
  expect_match(
    add_text(
      text = "ABC",
      header = "ABC",
      pre_header_text = ""
    ),
    "<a"
  )
})

test_that("Header not included if missing", {
  expect_no_match(
    add_text(
      text = "ABC",
      header = "",
      pre_header_text = ""
    ),
    "<a"
  )
})


test_that("Text included if present", {
  expect_match(
    add_text(
      text = "ABC",
      header = "",
      pre_header_text = "ABC"
    ),
    "ABC"
  )
})
