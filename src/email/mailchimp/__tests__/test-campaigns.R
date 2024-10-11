# Mock functions
mock_get_env <- mock("test@example.com", cycle = TRUE)
mock_mc_campaign_folder_id <- mock("mock-folder-id", cycle = TRUE)
mock_hs_local_true <- mock(TRUE, cycle = TRUE)
mock_hs_local_false <- mock(FALSE, cycle = TRUE)

# Start of tests
test_that("mc_add_campaign works correctly with local TRUE", {
  with_mock_api({
    with_envvar(
      new = c(HS_LOCAL = TRUE),{
        result <- mc_add_campaign(
          subject_line = "Subject",
          preview_text = "Preview",
          title = "Title",
          recipients = list(),
          template_id = "template-id",
          folder = "folder"
        )

        expect_equal(result, list(id = "test-id", url = "test-url"))
    })
  })
})

test_that("mc_add_campaign works correctly with local FALSE", {
  with_mock_api({
    with_envvar(
      new = c(HS_LOCAL = FALSE, HS_EMAIL = "test@example.com"),{
        result <- mc_add_campaign(
          subject_line = "Subject",
          preview_text = "Preview",
          title = "Title",
          recipients = list(),
          template_id = "template-id",
          folder = "folder"
        )

        expect_equal(result, list(id = "test-id", url = "test-url"))
      })
  })
})



    # Test when hs_local() is FALSE
    stub(mc_add_campaign, "hs_local$hs_local", mock_hs_local_false)
    stub(mc_add_campaign, "get_env$get_env", mock_get_env)
    stub(mc_add_campaign, "folders$mc_campaign_folder_id", mock_mc_campaign_folder_id)

    response <- mc_add_campaign("Subject", "Preview", "Title", list(list_id = "list-id"), "template-id", "folder")
    expect_equal(response, list(id = "campaign-id", url = list("https://example.com/campaign")))
    expect_called(mock_hs_local_false, 1)
    expect_called(mock_get_env, 1)
    expect_called(mock_mc_campaign_folder_id, 1)
  })
})

test_that("mc_send_campaign works correctly", {
  with_mock_api({
    # Test when hs_local() is TRUE
    stub(mc_send_campaign, "hs_local$hs_local", mock_hs_local_true)
    stub(mc_send_campaign, "httr2$req_dry_run", function(req) req)

    dry_run <- mc_send_campaign("campaign-id")
    expect_s3_class(dry_run, "httr2_request")
    expect_equal(dry_run$method, "POST")
    expect_equal(dry_run$url, "https://us14.api.mailchimp.com/3.0/campaigns/campaign-id/actions/send")
    expect_called(mock_hs_local_true, 1)

    # Clear mock call history
    mockery::stub_reset(mock_hs_local_true)

    # Test when hs_local() is FALSE
    stub(mc_send_campaign, "hs_local$hs_local", mock_hs_local_false)

    expect_no_error(mc_send_campaign("campaign-id"))
    expect_called(mock_hs_local_false, 1)
  })
})

test_that("mc_campaign_info works correctly", {
  with_mock_api({
    # Test when hs_local() is TRUE
    stub(mc_campaign_info, "hs_local$hs_local", mock_hs_local_true)
    stub(mc_campaign_info, "httr2$req_dry_run", function(req) req)

    dry_run <- mc_campaign_info("campaign-id")
    expect_s3_class(dry_run, "httr2_request")
    expect_equal(dry_run$method, "GET")
    expect_equal(dry_run$url, "https://us14.api.mailchimp.com/3.0/campaigns/campaign-id")
    expect_called(mock_hs_local_true, 1)

    # Clear mock call history
    mockery::stub_reset(mock_hs_local_true)

    # Test when hs_local() is FALSE
    stub(mc_campaign_info, "hs_local$hs_local", mock_hs_local_false)

    mock_response <- list(
      id = "campaign-id",
      web_id = 123456,
      type = "regular",
      status = "sent"
    )

    # Create a mock file for the GET request
    httptest2::mock_api(
      verb = "GET",
      path = "/3.0/campaigns/campaign-id",
      response = mock_response
    )

    result <- mc_campaign_info("campaign-id")
    expect_equal(result, mock_response)
    expect_called(mock_hs_local_false, 1)
  })
})
