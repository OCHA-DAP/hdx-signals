box::use(httr2)
box::use(purrr)
box::use(dplyr)
box::use(tidyr)
box::use(rlang)
box::use(countrycode)

# local modules
box::use(./base_api)
box::use(cs = ../../utils/cloud_storage)

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

#' Get Mailchimp interests for ISO3 codes
#'
#' Return all relevant Mailchimp interests for the ISO3 codes. The interests are UNHCR
#' regions and whether or not they are an HRP country.
#'
#' For the list of ISO3 codes, all relevant interests are returned that cover the
#' ISO3 codes. So if AFG and GBR are passed in, the European and Asian and the
#' Pacific regions would be returned, as well as HRP countries. This is to
#' ensure that email segmentation is sent to everyone interested in the
#' list of countries provided. By default, the IDs are returned, which can
#' be used for segmentation using `mc_group_conditions()`. Otherwise, full names
#' are returned which are used in conditional blocks.
#'
#' @param iso3 Vector of ISO3 codes
#' @param use End use. If `segmentation`, then IDs are returned which can be
#'     used in conditions. If `conditional_blocks`, then a comma separated
#'     string is returned.
#'
#' @returns Interest names string separated by commas or list of interest IDs
#'
#' @export
mc_interests_iso3 <- function(iso3,  use = c("segmentation", "conditional_blocks")) {
  use <- rlang$arg_match(use)
  regions <- countrycode$countrycode(iso3, origin = "iso3c", destination = "unhcr.region")
  hrp_countries <- cs$read_az_file("input/hrp_country.parquet")

  if (any(iso3 %in% hrp_countries$iso3)) {
    regions <- c(regions, "HRP countries")
  }

  regions <- sort(unique(regions))

  if (use == "segmentation") {
    mc_interests_ids(regions)
  } else {
    paste(regions, collapse = ",")
  }
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
