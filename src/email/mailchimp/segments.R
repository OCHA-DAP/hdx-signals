box::use(httr2)
box::use(purrr)
box::use(dplyr)
box::use(glue)
box::use(tidyr)

# local modules
box::use(./base_api)

indicators <- c("IDMC", "IPC")

httr2$request(
    "https://us14.api.mailchimp.com/3.0/lists/e908cb9d48/segments"
  ) |>
  httr2$req_auth_bearer_token(
    token = Sys.getenv("MAILCHIMP_API_KEY")
  ) |>
  httr2$req_body_json(
    data = list(
      name = "asdf",
      options = list(
        match = "all",
        conditions = list(
          list(
            match = "any",
            conditions = list(
              list(
                condition_type = "Interests",
                field = "interests-2f263932a3",
                op = "interestcontains",
                value = list("1314c9b26e")
              ),
              list(
                condition_type = "Interests",
                field = "interests-22b9c25441",
                op = "interestcontains",
                value = list("ab02282f78")
              )
            )
          ),
          list(
            match = "any",
            conditions = list(
              condition_type = "SelectMerge",
              field = "3",
              op = "contains",
              value = "concern"
            )
          )
        )
      )
    )
  ) |>
  httr2$req_perform() |>
  httr2$resp_body_json()

#' List available merge fields
#'
#' Gets list of the available merge fields in MailChimp.
mc_merge_fields <- function() {
  # get merge fields list
  response <- base_api$mc_api() |>
    httr2$req_url_path_append(
      "lists",
      "e908cb9d48",
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
      "lists",
      "e908cb9d48",
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
      "lists",
      "e908cb9d48",
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
      "lists",
      "e908cb9d48",
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
