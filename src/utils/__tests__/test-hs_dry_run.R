test_that("hs_dry_run works correctly", {
  with_envvar(new = c(HS_DRY_RUN = FALSE), {
    expect_false(hs_dry_run())
  })

  with_envvar(new = c(HS_DRY_RUN = TRUE), {
    expect_true(hs_dry_run())
  })

  with_envvar(new = c(HS_DRY_RUN = NA), {
    expect_true(hs_dry_run())
  })
})
