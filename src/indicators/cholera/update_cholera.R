box::use(cs = ../../utils/cloud_storage)
box::use(./utils/raw_cholera)
box::use(./utils/wrangle_cholera)
box::use(./utils/alert_cholera)
box::use(./utils/plot_cholera)
box::use(./utils/info_cholera)

box::use(../../alerts/generate_alerts[generate_alerts])
box::use(../../alerts/generate_campaigns[generate_campaigns])

df_wrangled <- raw_cholera$raw() |>
  wrangle_cholera$wrangle()

df_alerts <- df_wrangled |>
  alert_cholera$alert() |>
  generate_alerts(
    fn_df_alerts = "output/cholera/alerts.parquet",
    fn_df_campaigns = "output/cholera/campaigns.parquet",
    first_run = TRUE
  )

df_campaigns <- generate_campaigns(
  df_alerts = df_alerts,
  df_wrangled = df_wrangled,
  fn_df_campaigns = "output/cholera/campaigns.parquet",
  indicator_id = "who_cholera",
  plot_fn = plot_cholera$plot,
  info_fn = info_cholera$info,
  first_run = TRUE
)
