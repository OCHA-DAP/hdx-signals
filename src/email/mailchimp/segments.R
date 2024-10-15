box::use(
  httr2,
  rlang,
  uuid,
  purrr
)

box::use(src/email/mailchimp/base_api)

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

#' Add static segment to Mailchimp
#'
#' Add a static segment to Mailchimp. Requires you to pass in a list of
#' emails. Use if the segment doesn't already exist. Not used in monitoring
#' but is present in case there is any
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
