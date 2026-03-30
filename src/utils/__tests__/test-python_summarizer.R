box::use(
  src/utils/python_setup,
  reticulate
)

impl <- attr(python_setup, "namespace")

test_that("get_summary_r returns mocked value", {

  fn <- impl$get_summary_r

  mock_py_get_summary <- mock("Result from Python")

  fake_py <- list(
    get_summary = function(...) stop("Python error")
  )

  result <- fn(
    system_prompt = "sys",
    user_prompt   = "user",
    info          = "info",
    location      = NULL
  )

  expect_equal(result, "Result from Python")
  expect_called(mock_py_get_summary, n = 1)
})
