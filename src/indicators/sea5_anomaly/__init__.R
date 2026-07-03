#' @export
box::use(
  src/indicators/sea5_anomaly/utils/alert_sea5_anomaly[...],
  src/indicators/sea5_anomaly/utils/wrangle_sea5_anomaly[...],
  src/indicators/sea5_anomaly/utils/info_sea5_anomaly[...],
  src/indicators/sea5_anomaly/utils/plot_sea5_anomaly[...],
  src/indicators/sea5_anomaly/utils/map_sea5_anomaly[...],
  src/indicators/sea5_anomaly/utils/raw_sea5_anomaly[...],
  src/indicators/sea5_anomaly/utils/summary_sea5_anomaly[...],
)

#' @export
indicator_id <- "sea5_anomaly"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/sea5_anomaly,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("ETH", "SOM")
  )
}
