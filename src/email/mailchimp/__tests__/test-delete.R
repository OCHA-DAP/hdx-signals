test_that("mc_delete_object works correctly with local TRUE", {
  with_envvar(
    new = c(HS_LOCAL = TRUE, MAILCHIMP_API_KEY = "ABC"), {
      without_internet(
        expect_no_request(
          mc_delete_object(1)
        )
      )
    })
})

test_that("mc_delete_object works correctly with local FALSE", {
  with_envvar(
    new = c(HS_LOCAL = FALSE, MAILCHIMP_API_KEY = "ABC"), {
      without_internet({
        expect_DELETE(
          mc_delete_object(id = 1, object_type = "file"),
          "https://us14.api.mailchimp.com/3.0/file-manager/files/1"
        )

        expect_DELETE(
          mc_delete_object(id = 2, object_type = "template"),
          "https://us14.api.mailchimp.com/3.0/templates/2"
        )

        expect_DELETE(
          mc_delete_object(id = 3, object_type = "campaign"),
          "https://us14.api.mailchimp.com/3.0/campaigns/3"
        )
      })
    })
})
