box::use(idmc)

box::use(src/utils/get_env[get_env])

#' Download raw displacement data
#'
#' Downloads raw displacement data from the IDMC IDU. Uses the `{idmc}` package,
#' which requires the `IDMC_API` environment variables.
#'
#' @export
raw <- function() {
  get_env("IDMC_API", output = FALSE)
  idmc$idmc_get_data()
}
