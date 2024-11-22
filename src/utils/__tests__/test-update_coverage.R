test_that("update_coverage works correctly", {
  mock_update_az_file <- mock()
  stub(update_coverage, "cs$update_az_file", mock_update_az_file)

  update_coverage(
    indicator_id = "a",
    iso3 = c("ABC", "XYZ", "XYZ", "DEF")
  )

  expect_args(
    mock_object = mock_update_az_file,
    n = 1,
    df = data.frame(iso3 = c("ABC", "DEF", "XYZ")),
    name = "output/a/coverage.parquet"
  )
})
