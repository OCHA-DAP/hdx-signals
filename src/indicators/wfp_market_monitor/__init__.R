#' @export
box::use(
  src/indicators/wfp_market_monitor/utils/alert_market_monitor[...],
  src/indicators/wfp_market_monitor/utils/wrangle_market_monitor[...],
  src/indicators/wfp_market_monitor/utils/info_market_monitor[...],
  src/indicators/wfp_market_monitor/utils/plot_market_monitor[...],
  src/indicators/wfp_market_monitor/utils/raw_market_monitor[...],
  src/indicators/wfp_market_monitor/utils/summary_market_monitor[...],
  src/indicators/wfp_market_monitor/utils/wrangle_market_monitor[...]
)

#' @export
indicator_id <- "wfp_market_monitor"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/wfp_market_monitor,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("TCD", "SSD")
  )
}
