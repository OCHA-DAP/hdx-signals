box::use(httr2)
box::use(uuid)
box::use(utils)

box::use(./base_api)
box::use(../../utils/gmas_test_run)

#' Adds a template to Mailchimp
#'
#' Adds a template to Mailchimp. Just need to provide a name for the template
#' and the HTML for inclusion.
#'
#' @param name Name of the template to create
#' @param html Raw HTML for the template
#'
#' @returns ID of the created template
#'
#' @export
mc_add_template <- function(html) {
  if (gmas_test_run$gmas_test_run()) {
    writeLines(
      html,
      tf <- tempfile(fileext = ".html")
    )

    utils$browseURL(
      paste0("file://", tf)
    )
    "test-id"
  } else {
    response <- base_api$mc_api(lists_api = FALSE) |>
      httr2$req_url_path_append(
        "templates"
      ) |>
      httr2$req_body_json(
        data = list(
          name = uuid$UUIDgenerate(),
          html = html,
          folder_id = "9da634795f"
        )
      ) |>
      httr2$req_perform() |>
      httr2$resp_body_json()

    response$id
  }
}
