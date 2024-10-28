test_that("get_iso3_sf works correctly", {
  stub(get_iso3_sf, "cs$az_file_detect", c(
    "input/adm0/a.geojson",
    "input/adm0/b.geojson",
    "input/cities/a.geojson",
    "input/centroids/a.parquet",
    "input/centroids/b.parquet"
  ))

  mock_st_as_sf <- mock()
  mock_iso3_shift_longitude <- mock()
  stub(get_iso3_sf, "sf$st_as_sf", mock_st_as_sf)
  stub(get_iso3_sf, "iso3_shift_longitude$iso3_shift_longitude", mock_iso3_shift_longitude)
  stub(get_iso3_sf, "cs$read_az_file", mock())
  stub(get_iso3_sf, "cs$read_az_file_cached", mock())

  expect_null(get_iso3_sf("b", "cities")) # shouldn't exist
  expect_called(mock_iso3_shift_longitude, 0)

  get_iso3_sf("a", "adm0")
  expect_called(mock_iso3_shift_longitude, 1)
  expect_called(mock_st_as_sf, 0)

  get_iso3_sf("b", "centroids")
  expect_called(mock_iso3_shift_longitude, 2)
  expect_called(mock_st_as_sf, 1)
})
