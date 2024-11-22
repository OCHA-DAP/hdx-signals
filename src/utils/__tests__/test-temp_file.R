box::use(
  here,
  tools
)

test_that("temp_file works correctly", {
  tf <- temp_file(fileext = ".txt")
  expect_equal(
    tools$file_ext(tf),
    "txt"
  )

  expect_equal(
    basename(dirname(tf)),
    ".temp_dir"
  )

  writeLines(text = "abc", con = tf)

  expect_contains(
    list.files(path = here$here(".temp_dir")),
    basename(tf)
  )

  unlink(tf)
})
