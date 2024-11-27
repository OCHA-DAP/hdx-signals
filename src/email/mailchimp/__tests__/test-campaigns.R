test_that("mc_add_campaign works correctly with local TRUE", {
  with_envvar(
    new = c(HS_LOCAL = TRUE, MAILCHIMP_API_KEY = "ABC"),{
      result <- mc_add_campaign(
        subject_line = "Subject",
        preview_text = "Preview",
        title = "Title",
        recipients = list(),
        template_id = "template-id",
        folder = "1"
      )

      expect_equal(result, list(id = "test-id", url = "test-url"))
    })
})

test_that("mc_add_campaign works correctly with local FALSE", {
  with_mock_api({
    with_envvar(
      new = c(HS_LOCAL = FALSE, HS_EMAIL = "test@example.com", MAILCHIMP_API_KEY = "ABC"),{
        # Mock functions
        mock_mc_campaign_folder_id <- mock("1", cycle = TRUE)
        stub(mc_add_campaign, "folders$mc_campaign_folder_id", mock_mc_campaign_folder_id)

        result <- mc_add_campaign(
          subject_line = "Subject",
          preview_text = "Preview",
          title = "Title",
          recipients = list(),
          template_id = "1",
          folder = "1"
        )

        expect_equal(result, list(id = "campaign-id", url = "https://example.com/campaign"))
      })
  })
})

test_that("mc_send_campaign works correctly with local true", {
  without_internet({
    with_envvar(new = c(HS_LOCAL = TRUE, MAILCHIMP_API_KEY = "ABC"), {
      result <- mc_send_campaign("campaign-id")

      expect_equal(result$method, "POST")
      expect_equal(result$path, "/3.0/campaigns/campaign-id/actions/send")
    })
  })
})

test_that("mc_send_campaign works correctly with local FALSE", {
  without_internet({
    with_envvar(new = c(HS_LOCAL = FALSE, MAILCHIMP_API_KEY = "ABC"), {
      expect_POST(
        mc_send_campaign("campaign-id"),
        "https://us14.api.mailchimp.com/3.0/campaigns/campaign-id/actions/send"
      )
    })
  })
})

test_that("mc_campaign_info works correctly with local TRUE", {
  without_internet({
    with_envvar(new = c(HS_LOCAL = TRUE, MAILCHIMP_API_KEY = "ABC"), {
      result <- mc_campaign_info("campaign-id")

      expect_equal(result$method, "GET")
      expect_equal(result$path, "/3.0/campaigns/campaign-id")
    })
  })
})

test_that("mc_campaign_info works correctly with local FALSE", {
  without_internet({
    with_envvar(new = c(HS_LOCAL = FALSE, MAILCHIMP_API_KEY = "ABC"), {
      expect_GET(
        mc_campaign_info("campaign-id"),
        "https://us14.api.mailchimp.com/3.0/campaigns/campaign-id"
      )
    })
  })
})
