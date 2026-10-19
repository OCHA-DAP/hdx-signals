box::use(src/utils[...])

test_that("format_bullets() starts each newline separated bullet on a new line", {
  expect_equal(
    format_bullets("Actions:\n- Scale up aid.\n- Fund nutrition."),
    "Actions:<br>- Scale up aid.<br>- Fund nutrition."
  )
})

test_that("format_bullets() splits bullets that run together after a sentence", {
  expect_equal(
    format_bullets("Actions: - Scale up aid. - Fund nutrition."),
    "Actions:<br>- Scale up aid.<br>- Fund nutrition."
  )
})

test_that("format_bullets() handles other bullet characters", {
  expect_equal(
    format_bullets("Actions:\n* Scale up aid.\n\u2022 Fund nutrition."),
    "Actions:<br>- Scale up aid.<br>- Fund nutrition."
  )
})

test_that("format_bullets() leaves hyphens and dashes inside sentences alone", {
  text <- "Food-insecure households need cash - not only food - this season."
  expect_equal(format_bullets(text), text)
})

test_that("format_bullets() is vectorized and keeps NA", {
  expect_equal(
    format_bullets(c("No bullets here.", NA, "A:\n- b.")),
    c("No bullets here.", NA, "A:<br>- b.")
  )
})
