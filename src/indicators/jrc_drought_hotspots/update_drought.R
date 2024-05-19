box::use(./utils/raw_drought)
box::use(./utils/wrangle_drought)
box::use(./utils/alert_drought)
box::use(./utils/summary_drought)
box::use(./utils/info_drought)
box::use(./utils/plot_drought)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../alerts/triage_signals[triage_signals])

df_raw <- raw_drought$raw()
df_wrangled <- wrangle_drought$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "jrc_drought_hotspots",
  alert_fn = alert_drought$alert,
  summary_fn = summary_drought$summary,
  info_fn = info_drought$info,
  plot_fn = plot_drought$plot
)
