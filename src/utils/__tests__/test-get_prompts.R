test_that("get_prompts works correctly", {
  mock_readLines <- mock(c("a", "b"), c("c", "d"))
  stub(get_prompts, "readLines", mock_readLines)

  result <- get_prompts("abc")
  expect_equal(result, list(short = "a b", long = "c d"))
  expect_args(
    mock_object = mock_readLines,
    n = 1,
    con = file.path("src", "indicators", "abc", "prompts", "short.txt")
  )
  expect_args(
    mock_object = mock_readLines,
    n = 2,
    con = file.path("src", "indicators", "abc", "prompts", "long.txt")
  )
})
