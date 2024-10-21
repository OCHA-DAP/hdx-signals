box::use(
  src/indicators/acled_inform/utils/raw_inform,
  src/indicators/acled_inform/utils/wrangle_inform,
  src/indicators/acled_inform/utils/alert_inform,
  src/indicators/acled_inform/utils/plot_inform,
  src/indicators/acled_inform/utils/summary_inform,
  src/indicators/acled_inform/utils/map_inform,
  src/indicators/acled_inform/utils/info_inform,
  src/signals/generate_signals,
  src/utils/hs_logger,
  src/utils/update_coverage
)

dry_run_filter <- c("AFG", "SSD")
indicator_id <- "acaps_inform"

hs_logger$monitoring_log_setup(indicator_id)

df_raw <- raw_inform$raw()
df_wrangled <- wrangle_inform$wrangle(df_raw)

# update coverage for ipc
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

# now generate signals
generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = indicator_id,
  alert_fn = alert_inform$alert,
  plot_fn = plot_inform$plot,
  summary_fn = summary_inform$summary,
  map_fn = map_inform$map,
  info_fn = info_inform$info,
  dry_run_filter = dry_run_filter
)
