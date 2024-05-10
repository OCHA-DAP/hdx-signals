box::use(httr2)
box::use(purrr)
box::use(dplyr)

# local modules
box::use(./base_api)

#' Get members with specific tag
#'
#' Returns the emails of all subscribers with a specific tag. Useful because
#' for non-public indicators like cholera, subscribers are determined based
#' on tagging, and they can be targeted with static segments.
#'
#' @param tag_name Name of the tag
#'
#' @returns List of emails
#'
#' @export
mc_tag_members <- function(tag_name) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "members"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("members") |>
    purrr$keep(
      \(x) has_tag(x, tag_name)
    ) |>
    purrr$map(
      \(x) x$email_address
    )
}

#' Get all unique tags int he Mailchimp servers
#'
#' Used to find all the tags stored on Mailchimp, to use for filtering private
#' subscriptions.
#'
#' @export
mc_tags <- function() {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "members"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("members") |>
    purrr$map(
      .f = \(x) as.data.frame(x[["tags"]])
    ) |>
    dplyr$bind_rows() |>
    dplyr$pull(name) |>
    unique() |>
    sort()
}

#' Check member has tag
#'
#' Checks that member has the specified `tag_name`. Returns `TRUE` so can be
#' used for filtration in `purrr::keep()`.
#'
#' @param member_list Member list
#' @param tag_name Name of the tag as a string
#'
#' @returns Boolean
has_tag <- function(member_list, tag_name) {
  df_tags <- as.data.frame(member_list$tags)
  tag_name %in% df_tags$name
}
