box::use(
  src/indicators/who_cholera/utils/raw_cholera,
  src/indicators/who_cholera/utils/wrangle_cholera,
  src/indicators/who_cholera/utils/alert_cholera,
  src/indicators/who_cholera/utils/plot_cholera,
  src/indicators/who_cholera/utils/info_cholera,
  src/signals/generate_signals
)

# get the raw and wrangled data. The wrangled data is passed
# to generate up the new alerts and campaigns
df_wrangled <- raw_cholera$raw() |>
  wrangle_cholera$wrangle()

# now generate signals
generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "who_cholera",
  alert_fn = alert_cholera$alert,
  plot_fn = plot_cholera$plot,
  info_fn = info_cholera$info
)
