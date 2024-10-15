box::use(
  here
)

test_that("Runs and returns single string", {
  # create data frames to pass to email body
  df_details <- data.frame(
    title = "TITLE"
  )

  df_content <- data.frame(
    iso3 = "A",
    location = "B",
    plot_title = "C",
    plot_url = "D",
    map_title = "E",
    map_url = "F",
    plot2_title = "G",
    plot2_url = "H",
    other_images_urls = "I",
    other_images_titles = "J",
    summary_short = "K",
    summary_long = "L",
    further_info = "M"
  )

  # ensure working directory correctly set to load HTML
  local_dir(here$here())

  email_body <- create_body(
    campaign_details = df_details,
    df_campaign_content = df_content
  )

  expect_type(email_body, "character")
  expect_length(email_body, 1)
})
