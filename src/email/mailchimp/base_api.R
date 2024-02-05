box::use(httr2)

#' Base API call for Mailchimp
#'
#' Automatically sets up retrying 5 times, authentication, and the base URL for
#' other calls to build from. Sets the base API. If `lists` is true, appends the
#' `lists/{list_id}` automatically.
#'
#' @param lists_api If `TRUE`, base path is to the lists API.
#'
#' @export
mc_api <- function(lists_api = TRUE) {
  base_request <- httr2$request(
    "https://us14.api.mailchimp.com/3.0"
  ) |>
  httr2$req_retry(
    max_tries = 5
  ) |>
  httr2$req_auth_bearer_token(
    token = Sys.getenv("MAILCHIMP_API_KEY")
  )

  if (lists_api) {
    base_request <- httr2$req_url_path_append(
      base_request,
      "lists",
      "e908cb9d48"
    )
  }

  base_request
}
