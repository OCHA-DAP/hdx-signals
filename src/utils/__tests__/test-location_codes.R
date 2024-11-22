test_that("names_to_iso3 works correctly", {
  expect_equal(
    names_to_iso3(c("Afganistan", "South Sudan")),
    c("AFG", "SSD")
  )
  expect_equal(
    names_to_iso3(c("C. African Rep.", "Kosovo")),
    c("CAF", "XKX")
  )
  expect_warning(
    names_to_iso3("No name!")
  )
})

test_that("ison_to_iso3 works correctly", {
  expect_equal(
    ison_to_iso3(c(862, 840)),
    c("VEN", "USA")
  )
  expect_equal(
    ison_to_iso3(0),
    "XKX"
  )
  expect_warning(
    ison_to_iso3(555) # doesn't exist
  )
})

test_that("iso3_to_ison works correctly", {
  expect_equal(
    iso3_to_ison(c("SDN", "BFA")),
    c("729", "854")
  )
  expect_equal(
    iso3_to_ison("XKX"),
    "0"
  )
  expect_warning(
    iso3_to_ison("ABC") # doesn't exist
  )
})

test_that("iso2_to_iso3 works correctly", {
  expect_equal(
    iso2_to_iso3(c("NE", "TD")),
    c("NER", "TCD")
  )
  expect_equal(
    iso2_to_iso3("LAC"),
    "LAC"
  )
  expect_warning(
    iso2_to_iso3("FF") # doesn't exist
  )
})

test_that("iso3_to_names works correctly", {
  stub(iso3_to_names, "cs$read_az_file_cached", data.frame(
    iso3 = c("ABC", "XYZ", "NOP"),
    location = c("A-name", "X-name", "N-name")
  ))

  expect_equal(
    iso3_to_names(c("NOP", "ABC")),
    c("N-name", "A-name")
  )
})

test_that("iso3_to_regions works correctly", {
  stub(iso3_to_regions, "cs$read_az_file_cached", data.frame(
    iso3 = c("ABC", "XYZ", "NOP"),
    region = c("A-reg", "X-reg", "N-reg")
  ))

  expect_equal(
    iso3_to_regions(c("NOP", "XYZ")),
    c("N-reg", "X-reg")
  )
})

test_that("asap_to_iso3 works correctly", {
  stub(asap_to_iso3, "cs$read_az_file_cached", data.frame(
    asap0_id = 1:10,
    iso3 = letters[1:10]
  ))

  expect_equal(
    asap_to_iso3(c(1, 5, 3)),
    c("a", "e", "c")
  )
})

