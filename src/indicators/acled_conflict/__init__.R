#' @export
box::use(
  src/indicators/acled_conflict/utils/alert_conflict[...],
  src/indicators/acled_conflict/utils/wrangle_conflict[...],
  src/indicators/acled_conflict/utils/info_conflict[...],
  src/indicators/acled_conflict/utils/plot_conflict[...],
  src/indicators/acled_conflict/utils/map_conflict[...],
  src/indicators/acled_conflict/utils/raw_conflict[...],
  src/indicators/acled_conflict/utils/summary_conflict[...],
)

#' @export
indicator_id <- "acled_conflict"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/acled_conflict,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("AFG", "SSD")
  )
}
