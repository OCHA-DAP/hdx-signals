#' @export
box::use(
  src/indicators/idmc_displacement/utils/alert_displacement[...],
  src/indicators/idmc_displacement/utils/wrangle_displacement[...],
  src/indicators/idmc_displacement/utils/info_displacement[...],
  src/indicators/idmc_displacement/utils/plot_displacement[...],
  src/indicators/idmc_displacement/utils/map_displacement[...],
  src/indicators/idmc_displacement/utils/summary_displacement[...],
)

box::use(src/indicators/idmc_displacement/utils/raw_displacement)
box::use(dplyr)

#' @export
indicator_id <- "idmc_displacement_disaster"

#' Get raw displacement for specific indicator ID
#'
#' @export
raw <- function() {
  raw_displacement$raw() |>
    dplyr$filter(displacement_type == "Disaster")
}

if (is.null(box::name())) {
  box::use(
    module = src/indicators/idmc_displacement/disaster,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("AFG", "SSD")
  )
}
