box::use(
  httr2,
  rlang,
  stringr
)

box::use(
  src/email/mailchimp/base_api,
  src/utils/hs_local
)

#' Delete Mailchimp object
#'
#' Deletes Mailchimp object from the server based on its ID. Can be used to
#' delete files (images), templates, and campaigns. All deletion from Mailchimp
#' follows the same simple API structure of calling the URL path with its unique
#' ID and then pinging with the `DELETE` method.
#'
#' @param id Mailchimp ID for the object
#' @param object_type Type of object to delete, either a file, template, or campaign.
#'
#' @returns Nothing, object deleted from Mailchimp servers
#'
#' @export
mc_delete_object <- function(id, object_type = c("file", "template", "campaign")) {
  # get url path from object type
  object_type <- rlang$arg_match(object_type)

  if (object_type == "file") {
    url_path <- c("file-manager", "files")
  } else {
    url_path <- paste0(object_type, "s")
  }

  req <- base_api$mc_api(lists_api = FALSE) |>
    httr2$req_url_path_append(
      url_path,
      id
    ) |>
    httr2$req_method(
      "DELETE"
    )

  if (hs_local$hs_local()) {
    message(
      stringr$str_to_title(object_type),
      " not deleted because `hs_local()` is set to `TRUE`."
    )
  } else {
    # delete the template

    httr2$req_perform(req)
  }
  return(invisible(NULL))
}
