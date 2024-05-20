box::use(./utils/raw_agricultural_hotspots)
box::use(./utils/wrangle_agricultural_hotspots)
box::use(./utils/alert_agricultural_hotspots)
box::use(./utils/summary_agricultural_hotspots)
box::use(./utils/info_agricultural_hotspots)
box::use(./utils/plot_agricultural_hotspots)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../alerts/triage_signals[triage_signals])

df_raw <- raw_agricultural_hotspots$raw()
df_wrangled <- wrangle_agricultural_hotspots$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "jrc_agricultural_hotspots",
  alert_fn = alert_agricultural_hotspots$alert,
  summary_fn = summary_agricultural_hotspots$summary,
  info_fn = info_agricultural_hotspots$info,
  plot_fn = plot_agricultural_hotspots$plot
)
