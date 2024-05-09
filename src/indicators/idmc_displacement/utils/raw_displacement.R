box::use(idmc)
box::use(dplyr)

#' Download raw displacement data
#'
#' Downloads raw displacement data from the IDMC IDU. Uses the `{idmc}` package,
#' which requires the `IDMC_API` environment variables.
#'
#' @export
raw <- function() {
  idmc$idmc_get_data()
}
