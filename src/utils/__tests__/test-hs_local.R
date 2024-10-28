test_that("hs_local works correctly", {
  with_envvar(new = c(HS_LOCAL = FALSE), {
    expect_false(hs_local())
  })

  with_envvar(new = c(HS_LOCAL = TRUE), {
    expect_true(hs_local())
  })

  with_envvar(new = c(HS_LOCAL = NA), {
    expect_true(hs_local())
  })
})
