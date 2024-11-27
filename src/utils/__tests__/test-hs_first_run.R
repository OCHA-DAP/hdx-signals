test_that("hs_first_run works correctly", {
  with_envvar(new = c(HS_FIRST_RUN = FALSE), {
    expect_false(hs_first_run())
  })

  with_envvar(new = c(HS_FIRST_RUN = TRUE), {
    expect_true(hs_first_run())
  })

  with_envvar(new = c(HS_FIRST_RUN = NA), {
    expect_false(hs_first_run())
  })
})
