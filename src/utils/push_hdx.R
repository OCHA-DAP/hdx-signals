box::use(httr2)

box::use(src/utils/get_env)

#' Push data to HDX
#'
#' Triggers the HDX pipeline to upload data to HDX in the `hdx-signals` HDX
#' dataset as a resource. The pipeline is in the `OCHA-DAP/hdx-signals-alerts`
#' repository, and we simply create a webhook to run the pipeline. No data is
#' actually sent from R, the pipeline in that repository simply pulls from Azure
#' into HDX.
#'
#' @export
push_hdx <- function() {
  bearer <- get_env$get_env("HS_HDX_BEARER")
  link <- "https://api.github.com/repos/OCHA-DAP/hdx-signals-alerts/dispatches"

  httr2$request(link) |>
    httr2$req_body_json(
      data = list(
        event_type = "webhook"
      )
    ) |>
    httr2$req_method("POST") |>
    httr2$req_headers(
      accept = "application/vnd.github.v3+json"
    ) |>
    httr2$req_auth_bearer_token(
      token = bearer
    ) |>
    httr2$req_perform()
}
