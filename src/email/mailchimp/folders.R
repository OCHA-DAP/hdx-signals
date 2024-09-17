box::use(
  httr2,
  purrr,
  dplyr
)

box::use(
  src/email/mailchimp/base_api
)

#' Get id for Mailchimp template folder
#'
#' Takes in the name of a template folder and returns the id
#'
#' @param name Name of the template folder
#'
#' @export
mc_template_folder_id <- function(name) {
  mc_get_folder_id(
    name = name,
    url_path = "template-folders"
  )
}

#' Gets all template folders
#'
#' Finds all template folders on Mailchimp
#'
#' @export
mc_template_folders <- function() {
  mc_list_folders(
    url_path = c(
      "template-folders"
    )
  )
}

#' Get id for Mailchimp campaign folder
#'
#' Takes in the name of a campaign folder and returns the id
#'
#' @param name Name of the campaign folder
#'
#' @export
mc_campaign_folder_id <- function(name) {
  mc_get_folder_id(
    name = name,
    url_path = "campaign-folders"
  )
}

#' Gets all campaign folders
#'
#' Finds all campaign folders on Mailchimp
#'
#' @export
mc_campaign_folders <- function() {
  mc_list_folders(
    url_path = c(
      "campaign-folders"
    )
  )
}

#' Get id for Mailchimp file folder
#'
#' Takes in the name of a file folder and returns the id
#'
#' @param name Name of the file folder
#'
#' @export
mc_file_folder_id <- function(name) {
  mc_get_folder_id(
    name = name,
    url_path = c(
      "file-manager",
      "folders"
    )
  )
}

#' Gets all file folders
#'
#' Finds all file folders on Mailchimp
#'
#' @export
mc_file_folders <- function() {
  mc_list_folders(
    url_path = c(
      "file-manager",
      "folders"
    )
  )
}

#' Gets folder ID from name
#'
#' Pulls the list of folders from the API and gets the ID that matches the name
#'
#' @param name Name of the folder
#' @param url_path URL path for the API
mc_get_folder_id <- function(name, url_path) {
  df_folders <- mc_list_folders(url_path)

  if (!(name) %in% df_folders$name) {
    stop(
      "Template folder ",
      name,
      " does not exist.",
      call. = FALSE
    )
  }

  df_folders |>
    dplyr$filter(
      name == !!name
    ) |>
    dplyr$pull(id)
}

#' List Mailchimp folders
#'
#' Lists folders found at the URL path, either template or content folders
#'
#' @param url_path URL path for the API
mc_list_folders <- function(url_path) {
  base_api$mc_api(lists_api = FALSE) |>
    httr2$req_url_path_append(
      url_path
    ) |>
    httr2$req_url_query(
      count = 1000
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("folders") |>
    dplyr$bind_rows() |>
    dplyr$distinct(
      name, id
    )
}
