#' @export
box::use(
  src/indicators/acaps_inform_severity/utils/alert_inform[...],
  src/indicators/acaps_inform_severity/utils/wrangle_inform[...],
  src/indicators/acaps_inform_severity/utils/info_inform[...],
  src/indicators/acaps_inform_severity/utils/plot_inform[...],
  src/indicators/acaps_inform_severity/utils/table_inform[...],
  src/indicators/acaps_inform_severity/utils/raw_inform[...],
  src/indicators/acaps_inform_severity/utils/summary_inform[...],
)

#' @export
indicator_id <- "acaps_inform_severity"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/acaps_inform_severity,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("AFG", "NGA")
  )
}
