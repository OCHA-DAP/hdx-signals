box::use(
  src/utils/python_setup,
  reticulate
)
impl <- attr(python_setup, "namespace")

test_that("get_summary_r propagates python errors", {

  fn <- impl$get_summary_r

  # Inject fake Python module
  impl$reticulate <- list(
    py = list(
      get_summary = function(...) {
        stop("Missing environment variables")
      }
    )
  )

  expect_error(
    fn("sys", "user", "info"),
    "Missing environment variables"
  )
})
