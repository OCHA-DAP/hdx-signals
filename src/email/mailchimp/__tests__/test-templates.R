test_that("mc_add_template works correctly with local TRUE", {
  with_envvar(
    new = c(HS_LOCAL = TRUE),{
      without_internet({
        expect_no_request(
          mc_add_template(
            html = "",
            folder = 1,
            preview = FALSE
          )
        )

        mock_browseURL <- mock()
        stub(mc_add_template, "utils$browseURL", mock_browseURL)
        result <- mc_add_template(
          html = "",
          folder = 1,
          preview = TRUE
        )

        expect_equal(result, "test-id")
        expect_called(mock_browseURL, 1)
      })
    })
})

test_that("mc_add_template works correctly with local FALSE", {
  with_envvar(
    new = c(HS_LOCAL = FALSE, MAILCHIMP_API_KEY = "ABC"),{
      stub(mc_add_template, "folders$mc_template_folder_id", 1)
      stub(mc_add_template, "uuid$UUIDgenerate", 123)

      with_mock_api({
        result <- mc_add_template(
          html = "",
          folder = 1,
          preview = FALSE
        )

        expect_equal(result, "1")
      })
    })
})
