box::use(src/email/mailchimp/custom_segmentation)

box::use(purrr)

impl = attr(custom_segmentation, 'namespace')

test_that("mc_email_segment works correctly", {
  stub(mc_email_segment, "cs$read_az_file", data.frame(indicator_id = letters[1:3], static_segment = LETTERS[1:3]))
  stub(mc_email_segment, "mc_subscriber_emails", c("a@email.com", "b@email.com"))
  mock_mc_update_static_segment <- mock()
  stub(mc_email_segment, "segments$mc_update_static_segment", mock_mc_update_static_segment)

  mc_email_segment(indicator_id = "b", iso3 = "ABC")

  expect_args(
    mock_object = mock_mc_update_static_segment,
    n = 1,
    segment_id = "B",
    segment_name = "b",
    emails = list()
  )

  expect_args(
    mock_object = mock_mc_update_static_segment,
    n = 2,
    segment_id = "B",
    segment_name = "b",
    emails = list("a@email.com", "b@email.com")
  )
})

test_that("mc_subscriber_emails works correctly", {
  stub(impl$mc_subscriber_emails, "audience$mc_groups", data.frame(
    title = c("Region A", "Region A", "Region A", "Region B", "Dataset", "Dataset"),
    category_id = c("a", "a", "a", "b", "c", "c"),
    name = c("Location AX", "Location AY", "All locations in the region",  "All locations in the region", "Dataset A", "Dataset B"),
    interest_id = LETTERS[1:6]
  ))

  stub(impl$mc_subscriber_emails, "location_codes$iso3_to_regions", c("Region A", "Region B"))
  stub(impl$mc_subscriber_emails, "location_codes$iso3_to_names", c("Location AY", "Location BX"))
  mock_interest_emails <- mock()
  mock_tag_emails <- mock()

  stub(impl$mc_subscriber_emails, "interest_emails", mock_interest_emails)
  stub(impl$mc_subscriber_emails, "tag_emails", mock_tag_emails)

  impl$mc_subscriber_emails(
    df_ind = data.frame(mc_interest = "Dataset B", mc_tag = NA_character_),
    iso3 = c("AYY", "BXX")
  )

  expect_args(
    mock_object = mock_interest_emails,
    n = 1,
    interest = "Dataset B",
    geo_ids = c("C", "D", "B")
  )

  expect_called(mock_tag_emails, n = 0)

  impl$mc_subscriber_emails(
    df_ind = data.frame(mc_interest = NA_character_, mc_tag = "Tag"),
    iso3 = c("AYY", "BXX")
  )

  expect_args(
    mock_object = mock_tag_emails,
    n = 1,
    interest = "Tag",
    geo_ids = c("C", "D", "B")
  )

  expect_called(mock_interest_emails, n = 1)
})

test_that("interest_emails works correctly", {
  stub(impl$interest_emails, "audience$mc_groups", data.frame(
    title = c("Region A", "Region A", "Region A", "Region B", "Dataset", "Dataset"),
    category_id = c("a", "a", "a", "b", "c", "c"),
    name = c("Location AX", "Location AY", "All locations in the region",  "All locations in the region", "Dataset A", "Dataset B"),
    interest_id = LETTERS[1:6]
  ))

  stub(impl$interest_emails, "audience$mc_members", list(
    list(
      email_address = "everything@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = TRUE,
        "D" = TRUE,
        "E" = TRUE,
        "F" = TRUE
      ),
      tags = list()
    ),
    list(
      email_address = "only_a@example.com",
      interests = list(
        "A" = TRUE,
        "B" = FALSE,
        "C" = FALSE,
        "D" = FALSE,
        "E" = TRUE,
        "F" = FALSE
      ),
      tags = list()
    ),
    list(
      email_address = "nothing@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = FALSE,
        "D" = FALSE,
        "E" = FALSE,
        "F" = FALSE
      ),
      tags = list()
    ),
    list(
      email_address = "all_regions_data_b@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = TRUE,
        "D" = TRUE,
        "E" = FALSE,
        "F" = TRUE
      ),
      tags = list()
    ),
    list(
      email_address = "all_regions_data_a@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = TRUE,
        "D" = TRUE,
        "E" = TRUE,
        "F" = FALSE
      ),
      tags = list()
    ),
    list(
      email_address = "country_b_all_data@example.com",
      interests = list(
        "A" = FALSE,
        "B" = TRUE,
        "C" = FALSE,
        "D" = FALSE,
        "E" = TRUE,
        "F" = TRUE
      ),
      tags = list()
    ),
    list(
      email_address = "everything_tester@example.com",
      interests = list(
        "A" = TRUE,
        "B" = TRUE,
        "C" = TRUE,
        "D" = TRUE,
        "E" = TRUE,
        "F" = TRUE
      ),
      tags = list(list(name = "hdx-signals-test"))
    )
  ))

  with_envvar(
    new = c(HS_DRY_RUN = FALSE),{
      expect_equal( # country A and both regions, dataset A
        impl$interest_emails(
          interest = "Dataset A",
          geo_ids = c("A", "C", "D")
        ) |> purrr$discard(is.na),
        c(
          "everything@example.com",
          "only_a@example.com",
          "all_regions_data_a@example.com",
          "everything_tester@example.com"
        )
      )

      expect_equal( # country A and both regions, dataset B
        impl$interest_emails(
          interest = "Dataset B",
          geo_ids = c("A", "C", "D")
        ) |> purrr$discard(is.na),
        c(
          "everything@example.com",
          "all_regions_data_b@example.com",
          "everything_tester@example.com"
        )
      )

      expect_equal( # both regions, dataset A
        impl$interest_emails(
          interest = "Dataset A",
          geo_ids = c("C", "D")
        ) |> purrr$discard(is.na),
        c(
          "everything@example.com",
          "all_regions_data_a@example.com",
          "everything_tester@example.com"
        )
      )

      expect_equal( # region B, dataset B
        impl$interest_emails(
          interest = "Dataset B",
          geo_ids = c("D")
        ) |> purrr$discard(is.na),
        c(
          "everything@example.com",
          "all_regions_data_b@example.com",
          "everything_tester@example.com"
        )
      )

      expect_equal( # country B, dataset B
        impl$interest_emails(
          interest = "Dataset B",
          geo_ids = c("B", "C")
        ) |> purrr$discard(is.na),
        c(
          "everything@example.com",
          "all_regions_data_b@example.com",
          "country_b_all_data@example.com",
          "everything_tester@example.com"
        )
      )
  })

  with_envvar(new = c(HS_DRY_RUN = TRUE), {
    expect_equal( # ensure the test tag works
      impl$interest_emails(
        interest = "Dataset A",
        geo_ids = c("A", "B", "C", "D")
      ) |> purrr$discard(is.na),
      c(
        "everything_tester@example.com"
      )
    )
  })
})

test_that("tag_emails works correctly", {
  stub(impl$tag_emails, "audience$mc_members", list(
    list(
      email_address = "everything@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = TRUE,
        "D" = TRUE
      ),
      tags = list(
        list(
          name = "A"
        ),
        list(
          name = "B"
        )
      )
    ),
    list(
      email_address = "only_a@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = TRUE,
        "D" = TRUE
      ),
      tags = list(
        list(
          name = "A"
        )
      )
    ),
    list(
      email_address = "nothing@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = FALSE,
        "D" = FALSE
      ),
      tags = list()
    ),
    list(
      email_address = "only_b@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = TRUE,
        "D" = TRUE
      ),
      tags = list(
        list(
          name = "B"
        )
      )
    ),
    list(
      email_address = "country_b_interest_a@example.com",
      interests = list(
        "A" = FALSE,
        "B" = TRUE,
        "C" = FALSE,
        "D" = FALSE
      ),
      tags = list(
        list(
          name = "A"
        )
      )
    ),
    list(
      email_address = "country_a_interest_b@example.com",
      interests = list(
        "A" = TRUE,
        "B" = FALSE,
        "C" = FALSE,
        "D" = FALSE
      ),
      tags = list(
        list(
          name = "B"
        )
      )
    ),
    list(
      email_address = "everything_tester@example.com",
      interests = list(
        "A" = TRUE,
        "B" = TRUE,
        "C" = TRUE,
        "D" = TRUE
      ),
      tags = list(
        list(name = "A"),
        list(name = "B"),
        list(name = "hdx-signals-test")
      )
    ),
    list(
      email_address = "nothing_tester@example.com",
      interests = list(
        "A" = FALSE,
        "B" = FALSE,
        "C" = FALSE,
        "D" = FALSE
      ),
      tags = list(
        list(name = "hdx-signals-test")
      )
    )
  ))

  with_envvar(new = c(HS_DRY_RUN = FALSE), {
    expect_equal(
      impl$tag_emails( # tag A, both regions and country A
        interest_tag = "A",
        geo_ids = c("A", "C", "D")
      ) |> purrr$discard(is.na),
      c(
        "everything@example.com",
        "only_a@example.com",
        "everything_tester@example.com"
      )
    )

    expect_equal(
      impl$tag_emails( # tag B, country A
        interest_tag = "B",
        geo_ids = c("A", "C")
      ) |> purrr$discard(is.na),
      c(
        "everything@example.com",
        "only_b@example.com",
        "country_a_interest_b@example.com",
        "everything_tester@example.com"
      )
    )

    expect_equal(
      impl$tag_emails( # tag A, country B
        interest_tag = "A",
        geo_ids = c("B", "C")
      ) |> purrr$discard(is.na),
      c(
        "everything@example.com",
        "only_a@example.com",
        "country_b_interest_a@example.com",
        "everything_tester@example.com"
      )
    )

    expect_equal(
      impl$tag_emails( # region D, tag A
        interest_tag = "A",
        geo_ids = "D"
      ) |> purrr$discard(is.na),
      c(
        "everything@example.com",
        "only_a@example.com",
        "everything_tester@example.com"
      )
    )
  })

  with_envvar(new = c(HS_DRY_RUN = TRUE), {
    expect_equal(
      impl$tag_emails( # region D, tag A
        interest_tag = "A",
        geo_ids = "D"
      ) |> purrr$discard(is.na),
      c(
        "everything_tester@example.com"
      )
    )
  })
})
