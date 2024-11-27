# Only tests mc_upate_static_segment since the other functions are just there
# for convenience when developing.

test_that("mc_update_static_segments works correctly", {
  with_envvar(new = c(MAILCHIMP_API_KEY = "ABC"), {
    without_internet(
      expect_PATCH(
        mc_update_static_segment(segment_id = 17, segment_name = "a_b", emails = list("example@email.com")),
        "https://us14.api.mailchimp.com/3.0/lists/e908cb9d48/segments/17",
        '{\"name\":\"a_b\",\"static_segment\":[\"example@email.com\"]}'
      )
    )

    with_mock_api(
      expect_equal(
        mc_update_static_segment(segment_id = 17, segment_name = "a_b", emails = list("example@email.com")),
        17
      )
    )
  })
})
