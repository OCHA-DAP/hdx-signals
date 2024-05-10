box::use(./utils/raw_conflict)
box::use(./utils/wrangle_conflict)
box::use(./utils/alert_conflict)
box::use(./utils/plot_conflict)
box::use(./utils/map_conflict)
box::use(./utils/info_conflict)
box::use(./utils/summary_conflict)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../alerts/generate_alerts[generate_alerts])
box::use(../../alerts/triage_signals)
box::use(../../alerts/delete_campaign_content)

df_raw <- raw_conflict$raw()
df_wrangled <- wrangle_conflict$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = "acled_conflict",
  alert_fn = alert_conflict$alert,
  plot_fn = plot_conflict$plot,
  info_fn = info_conflict$info,
  map_fn = map_conflict$map,
  summary_fn = summary_conflict$summary
)
