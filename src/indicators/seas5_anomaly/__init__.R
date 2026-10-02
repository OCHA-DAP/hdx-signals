#' @export
box::use(
  src/indicators/seas5_anomaly/utils/alert_seas5_anomaly[...],
  src/indicators/seas5_anomaly/utils/wrangle_seas5_anomaly[...],
  src/indicators/seas5_anomaly/utils/info_seas5_anomaly[...],
  src/indicators/seas5_anomaly/utils/plot_seas5_anomaly[...],
  src/indicators/seas5_anomaly/utils/map_seas5_anomaly[...],
  src/indicators/seas5_anomaly/utils/raw_seas5_anomaly[...],
  src/indicators/seas5_anomaly/utils/summary_seas5_anomaly[...],
)

#' @export
indicator_id <- "seas5_anomaly"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/seas5_anomaly,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("ETH", "SOM")
  )
}
