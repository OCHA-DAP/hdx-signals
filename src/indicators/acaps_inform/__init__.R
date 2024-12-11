#' @export
box::use(
  src/indicators/acaps_inform/utils/alert_inform[...],
  src/indicators/acaps_inform/utils/wrangle_inform[...],
  src/indicators/acaps_inform/utils/info_inform[...],
  src/indicators/acaps_inform/utils/plot_inform[...],
  src/indicators/acaps_inform/utils/map_inform[...],
  src/indicators/acaps_inform/utils/raw_inform[...],
  src/indicators/acaps_inform/utils/summary_inform[...],
)

#' @export
indicator_id <- "acaps_inform"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/acaps_inform,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("AFG", "SSD")
  )
}
