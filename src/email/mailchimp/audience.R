box::use(httr2)
box::use(purrr)
box::use(dplyr)
box::use(tidyr)
box::use(rlang)

# local modules
box::use(src/email/mailchimp/base_api)

box::use(cs = src/utils/cloud_storage)
box::use(src/utils/location_codes)

#' Get members information
#'
#' Get full data on members from Mailchimp. Used to then manually filter
#' Mailchimp members.
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
    resp_members <- purrr$list_c(resp_members, mc_member_req(offset = offset)$members)
    offset <- offset + 1000
  }
  resp_members
}

#' Offset member requests to the Mailchimp API
mc_member_req <- function(offset = 0) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "members"
    ) |>
    httr2$req_url_query(
      count = 1000
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json()
}

#' Get interest ID list
#'
#' Gets a list of interest IDs for a set of names passed in as
#' `interest_names`. Searches through the entire interest list,
#' but can be subset to a specific `category_id` if necessary (
#' e.g. there are duplicate names across categories).
#'
#' @param interest_names Vector of interest names
#' @param category_id Optional category ID to filter `mc_groups()` by
#'
#' @returns List of interest IDs
#'
#' @export
mc_interests_ids <- function(interest_names, category_id = NULL) {
  # get the data frame of categories and interests and filter if necessary
  df_interest <- mc_groups()
  if (!is.null(category_id)) {
    df_interest <- dplyr$filter(df_interest, category_id == !!category_id)
  }

  # extract ids as named vector for subsetting
  interest_ids <- df_interest$interest_id
  names(interest_ids) <- df_interest$name

  # return as a list for passing into the API
  interest_ids[interest_names] |>
    unname() |>
    as.list()
}

#' Get merge fields ID
#'
#' @param field Merge field
#'
#' @returns ID for the merge field
#'
#' @export
mc_fields_ids <- function(field) {
  df <- mc_fields()
  df$id[match(field, df$name)]
}


#' List available merge fields
#'
#' Gets list of the available merge fields in MailChimp.
mc_merge_fields <- function() {
  # get merge fields list
  response <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "merge-fields"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("merge_fields")

  purrr$map(
    .x = response,
    .f = \(x) dplyr$tibble(name = x[["name"]], id = x[["merge_id"]])
  ) |>
    purrr$list_rbind()
}

#' List the merge field options for fields
mc_merge_options <- function(id) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "merge-fields",
      id
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("options") |>
    purrr$pluck("choices") |>
    as.character()
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

#' Lists all merge closed field options and responses on Mailchimp
#'
#' List all merge field options for closed responses. Drops open text options.
#' Useful for looping through to create segments programmatically, which can
#' then be extracted
#'
#' @export
mc_fields <- function() {
  fields <- mc_merge_fields()
  fields |>
    dplyr$mutate(
      options = purrr$map(
        .x = id,
        .f = mc_merge_options
      )
    ) |>
    tidyr$unnest(options)
}
