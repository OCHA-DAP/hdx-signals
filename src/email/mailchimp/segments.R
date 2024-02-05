box::use(httr2)
box::use(rlang)
box::use(uuid)
box::use(purrr)

# local modules
box::use(./base_api)

#' Create conditions list for groups
#'
#' Creates a conditions list for groups, using the `interestcontains` operator.
#' Will create a condition that captures all members in those segments.
#'
#' @param category_id String of the category ID
#' @param segment_ids List of segment IDs.
#'
#' @export
mc_group_conditions <- function(category_id, segment_ids) {
  list(
    condition_type = "Interests",
    field = paste0("interests-", category_id),
    op = "interestcontains",
    value = segment_ids
  )
}

#' Create conditions list for merge fields
#'
#' Using a merge field ID, creates a condition list based on a value string
#' to search for. Allows using the `contains` and `is` operator. The advantage
#' is that we can use the `contains` operator to capture segments of people
#' with merge fields with similar strings.
#'
#' @param field_id String of the merge field ID
#' @param value_string String to search for
#' @param op Either `contains` or `is`, determining the text search.
#'
#' @export
mc_merge_conditions <- function(
    field_id,
    value_string,
    op = c("contains", "is")
) {
  op <- rlang$arg_match(op)
  list(
    condition_type = "SelectMerge",
    field = field_id,
    op = op,
    value = value_string
  )
}

#' Add segment to Mailchimp
#'
#' Add a segment to Mailchimp. Requires you to pass in a `conditions` list and
#' `match_option`. Defaults to using `all`, since that is how the majority of HDX
#' Signals segmentation will work, but `any` is option in case desired. Returns
#' the segment `id`.
#'
#' @param conditions List of conditions to pass to API
#' @param match_option `any` or `all`.
mc_add_segment <- function(conditions, match_option = c("all", "any")) {
  match_option <- rlang$arg_match(match_option)

  response <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "segments"
    ) |>
    httr2$req_body_json(
      data = list(
        name = uuid$UUIDgenerate(),
        options = list(
          match = match_option,
          conditions = conditions
        )
      )
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json()

  response$id
}

#' Get segment information
#'
#' Get segment information for a specific `segment_id`.
#'
#' @param segment_id Segment ID
mc_get_segment <- function(segment_id) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "segments",
      segment_id
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json()
}

#' Get segment conditions
#'
#' Get segment conditions for specific `segment_id`.
#'
#' @param segment_id Segment ID
#'
#' @return Nested lists of conditions
mc_get_segment_conditions <- function(segment_id) {
  mc_get_segment(segment_id) |>
    purrr$pluck("options") |>
    purrr$pluck("conditions")
}

#' Get segment members
#'
#' Get segment members for specific `segment_id`, returns their email addresses.
#' Useful if more complex segmentation needs to occur where static segments
#' are created and passed in programmatically, and for checking segment updating
#' rules.
#'
#' @param segment_id Segment ID
mc_get_segment_members <- function(segment_id) {
  members_list <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "segments",
      segment_id,
      "members"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("members")

  purrr$map_chr(
    .x = members_list,
    .f = \(x) x$email_address
  )
}

#' Check conditions equal
#'
#' Checks that two sets of conditions are equal. Does this in a very simplistic
#' manner by just checking that exactly an identical nested list is returned
#' from the API, thus misses any reordering of values or conditions. However,
#' works for this setup since conditions will be programmatically created in the
#' same order every time. Only even functionalizing this in case more complex
#' logic needs to be placed within the function, and to allow for this documentation.
#'
#' @param conditions1
#' @param conditions2
check_conditions_equal <- function(conditions1, conditions2) {
  all.equal(conditions1, conditions2)
}
