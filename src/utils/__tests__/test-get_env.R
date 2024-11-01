box::use(logger)

test_that("get_env works correctly", {
  with_envvar(new = c(ABC = "a", DEF = 1, GHI = NA), {
    expect_equal(get_env("ABC"), "a")
    expect_equal(get_env("DEF"), "1")
    expect_null(get_env("ABC", FALSE))
    # don't show the logging when testing
    original_threshold <- logger$log_threshold()
    logger$log_threshold("FATAL")
    expect_error(get_env("GHI"))
    # reset logging after running tests
    logger$log_threshold(original_threshold)
  })
})
