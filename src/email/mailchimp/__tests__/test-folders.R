box::use(dplyr)
box::use(src/email/mailchimp/folders)

impl = attr(folders, 'namespace')

df_folders <- dplyr$tibble(name = letters[1:3], id = 1:3)

test_that("mc_list_folders works properly", {
  with_mock_api({
    result <- impl$mc_list_folders("folder-path")
    expect_equal(result, df_folders)
  })

  without_internet(
    expect_GET(
      impl$mc_list_folders("folder-b"),
      "https://us14.api.mailchimp.com/3.0/folder-b?count=1000"
    )
  )
})

test_that("mc_get_folder_id works properly", {
  stub(impl$mc_get_folder_id, "mc_list_folders", df_folders)
  expect_equal(impl$mc_get_folder_id("a"), 1)
  expect_equal(impl$mc_get_folder_id("c"), 3)
})
