box::use(idmc)
box::use(dplyr)

#' Download raw displacement data
#'
#' Downloads raw displacement data from the IDMC IDU. Uses the `{idmc}` package,
#' which requires the `ACLED_EMAIL_ADDRESS` and `ACLED_ACCESS_KEY` environment
#' variables.
#'
#' @export
raw <- function() {
  idmc$idmc_get_data() |>
    dplyr$filter(
      displacement_type == "Conflict"
    )
}
