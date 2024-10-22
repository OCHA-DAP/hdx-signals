box::use(
  stringr
)

stub(conditional_merge, "location_codes$iso3_to_regions", "Region A")
stub(conditional_merge, "location_codes$iso3_to_names", "Name B")

test_that("Use conditions creates conditions string", {
  merge_txt <- conditional_merge(text = "Text XYZ", iso3 = "ABC", use_conditions = TRUE)

  expect_length(merge_txt, 1)
  # check substrings contained correct # of times
  expect_true(stringr$str_count(merge_txt, "Text XYZ") == 2)
  expect_true(stringr$str_count(merge_txt, "Region A") == 3)
  expect_true(stringr$str_count(merge_txt, "Name B") == 2)
})

test_that("Text just passed straight out if use conditions false", {
  txt <- "Test text!"
  expect_equal(conditional_merge(text = txt, iso3 = "ABC", use_conditions = FALSE), txt)
})
