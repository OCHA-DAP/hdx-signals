box::use(./utils/raw_cholera)
box::use(./utils/wrangle_cholera)
box::use(./utils/alert_cholera)
box::use(./utils/summary_cholera)
box::use(./utils/plot_cholera)
box::use(./utils/info_cholera)

box::use(../../utils/hs_logger)
box::use(../../signals/generate_signals)

dry_run_filter <- c("ETH", "SSD")
indicator_id <- "who_cholera"

hs_logger$monitoring_log_setup(indicator_id)

df_raw <- raw_cholera$raw()
df_wrangled <- wrangle_cholera$wrangle(df_raw)

# now generate signals
generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = indicator_id,
  alert_fn = alert_cholera$alert,
  plot_fn = plot_cholera$plot,
  summary_fn = summary_cholera$summary,
  info_fn = info_cholera$info,
  dry_run_filter = dry_run_filter
)
