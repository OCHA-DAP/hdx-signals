box::use(./utils/raw_drought)
box::use(./utils/wrangle_drought)
box::use(./utils/alert_drought)

box::use(../../alerts/generate_signals[generate_signals])

first_run <- TRUE

df_raw <- raw_drought$raw()
df_wrangled <- wrangle_drought$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "jrc_drought_hotspots",
  alert_fn = alert_drought$alert,
  first_run = first_run
)
