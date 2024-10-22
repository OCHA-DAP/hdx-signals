box::use(dplyr)

test_that("Test call is done once if members < 1000", {
  mock_member_req <- mock(
    list(
      total_items = 800,
      members = list(
        list(
          name = "A"
        )
      )
    )
  )

  stub(mc_members, "mc_member_req", mock_member_req)

  expect_equal(
    mc_members(),
    list(
      list(
        name = "A"
      )
    )
  )

  expect_called(mock_member_req, 1)
})



test_that("Test call is repeated if members > 1000", {
  mock_member_req <- mock(
    list(
      total_items = 1500,
      members = list(
        list(
          name = "A"
        )
      )
    ),
    list(
      total_items = 1500,
      members = list(
        list(
          name = "B"
        )
      )
    )
  )

  stub(mc_members, "mc_member_req", mock_member_req)

  expect_equal(
    mc_members(),
    list(
      list(
        name = "A"
      ),
      list(
        name = "B"
      )
    )
  )

  expect_called(mock_member_req, 2)
})

test_that("Check that mc_groups works properly", {
  mock_interests <- mock(
    dplyr$tibble(
      name = c("Interest A", "Interest B"),
      interest_id = c("1", "2")
    ),
    dplyr$tibble(
      name = c("Interest C", "Interest D"),
      interest_id = c("3", "4")
    )
  )

  stub(mc_groups, "mc_categories", dplyr$tibble(title = c("A", "B"), category_id = c("X", "Y")))
  stub(mc_groups, "mc_interests", mock_interests)

  expect_equal(
    mc_groups(),
    dplyr$tibble(
      title = c("A", "A", "B", "B"),
      category_id = c("X", "X", "Y", "Y"),
      name = paste("Interest", LETTERS[1:4]),
      interest_id = as.character(1:4)
    )
  )
})
