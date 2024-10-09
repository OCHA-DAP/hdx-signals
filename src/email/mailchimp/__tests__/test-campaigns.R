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
      new = c("HS_LOCAL" = FALSE, "HS_EMAIL" = "a@g.co"),
      with_mock_api({
        stub(mc_add_campaign, "folders$mc_campaign_folder", 2)
        response <- mc_add_campaign(
          subject_line = "Subject",
          preview_text = "Preview",
          title = "Title",
          recipients = list("a"),
          template_id = "1",
          folder = "Folder"
        )

        expect_GET({
          response
        },
        content = list(
          list(
            type = "regular",
            recipients = list("a"),
            settings = list(
              title = "Title",
              subject_line = "Subject",
              preview_text = "Preview",
              from_name = "HDX Signals",
              reply_to =  "a@g.co",
              template_id = 1,
              folder_id = 2
            ),
            tracking = list(
              opens = TRUE,
              html_clicks = TRUE,
              text_clicks = TRUE
            )
          )
        )
        )
      })

    )
  })
})

