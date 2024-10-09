without_internet({
  test_that("Test list returned if local", {
    with_envvar(
      new = c("HS_LOCAL" = TRUE),
      expect_equal(
        mc_add_campaign(),
        list(id = "test-id", url = "test-url")
      )
    )
  })

  test_that("Arguments correctly passed through to call", {
    with_envvar(
      new = c("HS_LOCAL" = FALSE),
      expect_POST(
        mc_add_campaign(
          subject_line = "Subject",
          preview_text = "Preview",
          title = "Title",
          recipients = list("a"),
          template_id = "ID"
        ),
      )
    )
  })
})

