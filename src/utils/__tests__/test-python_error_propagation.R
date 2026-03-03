box::use(
  src/utils/python_setup,
  reticulate
)

impl <- attr(python_setup, "namespace")

test_that("get_summary_r propagates python errors", {

  fn <- impl$get_summary_r

  mock_py_get_summary <- mock(
    stop("Missing environment variables")
  )

  stub(
    fn,
    "reticulate::py$get_summary",
    mock_py_get_summary
  )

  expect_error(
    fn("sys", "user", "info"),
    "Missing environment variables"
  )
})
