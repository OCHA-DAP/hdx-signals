box::use(src/email/mailchimp/images)

mock_encode_image <- mock("abc", cycle = TRUE)
mock_mc_test_image_view <- mock("https://example.com/image-mock", cycle = TRUE)

test_that("mc_upload_image works properly", {
  stub(mc_upload_image, "encode_image", mock_encode_image)
  stub(mc_upload_image, "folders$mc_file_folder_id", 1)

  with_envvar(new = c(HS_LOCAL = FALSE, MAILCHIMP_API_KEY = "ABC"),{
    with_mock_api({
      result <- mc_upload_image(fp = "folder-path", name = "name", folder = "1")
      expect_equal(result, data.frame(id = "1", url = "https://example.com/image"))
    })

    without_internet(
      expect_POST(
        mc_upload_image(fp = "folder-path", name = "name", folder = "1"),
        "https://us14.api.mailchimp.com/3.0/file-manager/files",
        "{\"file_data\":\"abc\",\"name\":\"name\",\"folder_id\":1}"
      )
    )
  })

  with_envvar(new = c(HS_LOCAL = TRUE), {
    stub(mc_upload_image, "mc_test_image_view", mock_mc_test_image_view)
    result <- mc_upload_image(fp = "folder-path", name = "name", folder = "1")
    expect_equal(result, data.frame(id = "test-image-id", url = "https://example.com/image-mock"))
  })
})

test_that("mc_upload_plot works properly", {
  mock_ggsave <- mock()
  mock_knitr_crop <- mock()
  mock_mc_upload_image <- mock()

  stub(mc_upload_plot, "temp_file$temp_file", "abc")
  stub(mc_upload_plot, "ggplot2$ggsave", mock_ggsave)
  stub(mc_upload_plot, "knitr$crop", mock_knitr_crop)
  stub(mc_upload_plot, "mc_upload_image", mock_mc_upload_image)

  mc_upload_plot(plot = "p", name = "name", folder = "1", height = 6, width = 4, crop = FALSE, preview = FALSE)
  expect_args(
    mock_object = mock_ggsave,
    n = 1,
    filename = "abc",
    plot = "p",
    height = 6,
    width = 4,
    unit = "in",
    dpi = 300
  )

  expect_called(mock_knitr_crop, 0)
  expect_args(
    mock_object = mock_mc_upload_image,
    n = 1,
    fp = "abc",
    name = "name",
    folder = "1",
    preview = FALSE
  )
})
