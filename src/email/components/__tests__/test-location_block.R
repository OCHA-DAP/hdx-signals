box::use(
  here
)

test_that("Runs and returns single string", {
  # create empty arguments and mock
  iso3 <- "ABC"
  location <- "XYZ"
  stub(add_location, "conditional_merge$conditional_merge", function(x) x)

  # set working directory to overall branch dir so HTML is loaded correctly
  local_dir(here$here())

  template <- create_template(
    body = "A",
    banner_url = "B"
  )


  expect_s3_class(template, "glue")
  expect_length(template, 1)
})
