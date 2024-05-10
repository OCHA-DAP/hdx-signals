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

#' Create conditions list for static segment
#'
#' Creates a conditions list for all members of a static segment.
#'
#' @param segment_id String of the segment ID
#'
#' @export
mc_static_conditions <- function(segment_id) {
  list(
    condition_type = "StaticSegment",
    field = "static_segment",
    op = "static_is",
    value = segment_id
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

#' Add static segment to Mailchimp
#'
#' Add a static segment to Mailchimp. Requires you to pass in a list of
#' emails.
#'
#' @param segment_name Name of the segment to create
#' @param emails List of emails to pass to segment
#'
#' @returns ID of the created segment
#'
#' @export
mc_add_static_segment <- function(segment_name, emails) {
  response <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "segments"
    ) |>
    httr2$req_body_json(
      data = list(
        name = segment_name,
        static_segment = emails
      )
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json()

  response$id
}

#' Update static segment to Mailchimp
#'
#' Update a static segment to Mailchimp. Requires you to pass in a list of
#' emails.
#'
#' @param segment_id ID of the segment
#' @param segment_name Name of the segment to create
#' @param emails List of emails to pass to segment
#'
#' @returns ID of the created segment
#'
#' @export
mc_update_static_segment <- function(segment_id, segment_name, emails) {
  response <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "segments",
      segment_id
    ) |>
    httr2$req_body_json(
      data = list(
        name = segment_name,
        static_segment = emails
      )
    ) |>
    httr2$req_method("PATCH") |>
    httr2$req_perform() |>
    httr2$resp_body_json()

  response$id
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
#'
#' @export
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

#' Find segment based on name
#'
#' Searches existing segments by name, and if a match is found, returns the ID
#'
#' @param segment_name Segment name to search for
#'
#' @returns Segment ID
#'
#' @export
mc_find_segment <- function(segment_name) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "segments"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("segments") |>
    purrr$keep(
      \(x) x$name == segment_name
    ) |>
    purrr$pluck(1, "id")
}

#' Get segment members
#'
#' Get members of a segment.
#'
#' @param segment_id Segment ID
#'
#' @returns Member count
#'
#' @export
mc_segment_member_count <- function(segment_id) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "segments",
      segment_id
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("member_count")
}
