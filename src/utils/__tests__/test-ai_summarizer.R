box::use(src/utils/ai_summarizer)

impl <- attr(ai_summarizer, 'namespace')

test_that("get_ai_summary runs regularly", {
  fn <- impl$get_ai_summary

  mock_create_chat_completion <- mock(
    list(
      choices = list(
        message.content = "Result!"
      )
    ),
    cycle = TRUE
  )

  # Create a standalone stub function before the environment blocks
  stub(fn, "openai$create_chat_completion", mock_create_chat_completion)

  with_envvar(new = c(HS_LOCAL = TRUE), {
    expect_equal(fn("a", "b"), "Test output.")
  })

  # TODO: get this functioning, unclear why it isn't?
  # with_envvar(new = c(HS_LOCAL = FALSE, OPENAI_API_KEY = "KEY"), {
  #   result <- fn("a", "b")
  #   expect_called(mock_create_chat_completion, n = 1)
  #   expect_equal(result, "Result!")
  # })
})
