box::use(./utils/raw_cholera)
box::use(./utils/wrangle_cholera)
box::use(./utils/alert_cholera)
box::use(./utils/plot_cholera)
box::use(./utils/info_cholera)

box::use(../../alerts/generate_signals[generate_signals])

# get the raw and wrangled data. The wrangled data is passed
# to generate up the new alerts and campaigns
df_wrangled <- raw_cholera$raw() |>
  wrangle_cholera$wrangle()

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "who_cholera",
  alert_fn = alert_cholera$alert,
  plot_fn = plot_cholera$plot,
  info_fn = info_cholera$info
)
