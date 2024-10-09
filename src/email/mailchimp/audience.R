box::use(
  httr2,
  purrr,
  dplyr,
  tidyr
)

box::use(
  src/email/mailchimp/base_api
)

#' Get members information
#'
#' Get full data on members from Mailchimp. Used to then manually filter
#' Mailchimp members. Calls `mc_member_req()` multiple times as the API limits
#' returned values to 1,000 each call.
#'
#' @returns List of Mailchimp members information
#'
#' @export
mc_members <- function() {
  resp <- mc_member_req()
  total_items <- resp$total_items
  offset <- 1000
  resp_members <- resp$members
  while (total_items > offset) {
    resp_members <- c(resp_members, mc_member_req(offset = offset)$members)
    offset <- offset + 1000
  }
  resp_members
}

#' List all categories and interests on Mailchimp
#'
#' Lists all group categories and interests stored on Mailchimp. Useful
#' for looping through to create segments programmatically, which can then
#' be extracted
#'
#' @export
mc_groups <- function() {
  categories <- mc_categories()
  categories |>
    dplyr$mutate(
      interests = purrr$map(
        .x = category_id,
        .f = mc_interests
      )
    ) |>
    tidyr$unnest(interests)
}

#' List interests in a category
#'
#' @param id Interest category ID
mc_interests <- function(id) {
  response <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "interest-categories",
      id,
      "interests"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("interests")

  purrr$map(
    .x = response,
    .f = \(x) dplyr$tibble(name = x[["name"]], interest_id = x[["id"]])
  ) |>
    purrr$list_rbind()
}

#' Offset member requests to the Mailchimp API
mc_member_req <- function(offset = 0) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "members"
    ) |>
    httr2$req_url_query(
      count = 1000,
      offset = offset
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json()
}

#' List Mailchimp interest categories
mc_categories <- function() {
  response <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "interest-categories"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("categories")

  purrr$map(
    .x = response,
    .f = \(x) dplyr$tibble(title = x[["title"]], category_id = x[["id"]])
  ) |>
    purrr$list_rbind()
}
