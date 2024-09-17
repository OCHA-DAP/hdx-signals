box::use(
  src/indicators/wfp_market_monitor/utils/raw_market_monitor,
  src/indicators/wfp_market_monitor/utils/wrangle_market_monitor,
  src/indicators/wfp_market_monitor/utils/alert_market_monitor,
  src/indicators/wfp_market_monitor/utils/plot_market_monitor,
  src/indicators/wfp_market_monitor/utils/info_market_monitor,
  src/indicators/wfp_market_monitor/utils/summary_market_monitor,
  src/signals/generate_signals,
  src/utils/hs_logger,
  src/utils/update_coverage
)

dry_run_filter <- c("SYR", "SSD")
indicator_id <- "wfp_market_monitor"

hs_logger$monitoring_log_setup(indicator_id)

# get data
df_raw <- raw_market_monitor$raw()
df_wrangled <- wrangle_market_monitor$wrangle(df_raw)

# update coverage for ipc
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

# now generate signals
generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = "wfp_market_monitor",
  alert_fn = alert_market_monitor$alert,
  plot_fn = plot_market_monitor$plot,
  info_fn = info_market_monitor$info,
  summary_fn = summary_market_monitor$summary,
  dry_run_filter = dry_run_filter
)
