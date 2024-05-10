box::use(./utils/raw_drought)
box::use(./utils/wrangle_drought)
box::use(./utils/alert_drought)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../alerts/triage_signals)
box::use(../../alerts/delete_campaign_content)

df_raw <- raw_drought$raw()
df_wrangled <- wrangle_drought$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "jrc_drought_hotspots",
  alert_fn = alert_drought$alert,
  test = TRUE,
  test_filter = c("ETH", "COD")
)

triage_signals$triage_signals(
  indicator_id = "jrc_drought_hotspots",
  test = TRUE
)
