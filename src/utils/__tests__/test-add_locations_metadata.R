stub(add_locations_metadata, "cs$read_az_file_cached", data.frame(
  iso3 = c("AAA", "BBB", "CCC", "DDD"),
  location = letters[1:4],
  region = LETTERS[1:4],
  hrp_location = c(TRUE, FALSE, TRUE, FALSE),
  boundary_source = "Source",
  lat = 1:4,
  lon = 5:8
))

test_that("add_locations_metadata correctly adds to 1 row df", {
  result <- add_locations_metadata(data.frame(iso3 = "BBB", x = 1))

  expect_contains(names(result), "x") # keeps old columns
  expect_equal(nrow(result), 1) # doesn't add or delete rows
  expect_equal(result$lat, 2) # join is correct
})

test_that("add_locations_metadata correctly adds to 3 row df", {
  result <- add_locations_metadata(data.frame(iso3 = c("AAA", "BBB", "CCC"), x = 1:3))

  expect_contains(names(result), "x") # keeps old columns
  expect_equal(nrow(result), 3) # doesn't add or delete rows
  expect_equal(result$lat, 1:3) # join is correct
})

test_that("add_locations_metadata correctly adds with duplicate rows of ISO3", {
  result <- add_locations_metadata(data.frame(iso3 = c("AAA", "AAA", "CCC"), x = 1:3))

  expect_contains(names(result), "x") # keeps old columns
  expect_equal(nrow(result), 3) # doesn't add or delete rows
  expect_equal(result$lat, c(1, 1, 3)) # join is correct
})

test_that("add_locations_metadata throws error if ISO3 not covered", {
  expect_error(
    add_locations_metadata(data.frame(iso3 = "A"))
  )

  expect_error(
    add_locations_metadata(data.frame(iso3 = c("AAA", "BBB", "XDX"), x = 1:3))
  )
})
