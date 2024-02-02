box::use(httr2)

#' Base API call for Mailchimp
#'
#' Automatically sets up retrying 5 times, authentication, and the base URL for
#' other calls to build from.
#'
#' @export
mc_api <- function() {
  httr2$request(
    "https://us14.api.mailchimp.com/3.0"
  ) |>
  httr2$req_retry(
    max_tries = 5
  ) |>
  httr2$req_auth_bearer_token(
    token = Sys.getenv("MAILCHIMP_API_KEY")
  )
}
