box::use(dplyr)

box::use(./utils/raw_displacement)
box::use(./utils/wrangle_displacement)
box::use(./utils/alert_displacement)
box::use(./utils/plot_displacement)
box::use(./utils/info_displacement)
box::use(./utils/summary_displacement)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../alerts/triage_signals)
box::use(../../alerts/delete_campaign_content)

df_raw <- raw_displacement$raw()
df_wrangled <- wrangle_displacement$wrangle(df_raw)

# now generate signals individually for each displacement type
df_conflict <- generate_signals(
  df_wrangled = dplyr$filter(df_wrangled, displacement_type == "Conflict"),
  df_raw = dplyr$filter(df_raw, displacement_type == "Conflict"),
  indicator_id = "idmc_displacement_conflict",
  alert_fn = alert_displacement$alert,
  plot_fn = plot_displacement$plot,
  info_fn = info_displacement$info,
  summary_fn = summary_displacement$summary,
  first_run = TRUE
)

generate_signals(
  df_wrangled = dplyr$filter(df_wrangled, displacement_type == "Disaster"),
  df_raw = dplyr$filter(df_raw, displacement_type == "Disaster"),
  indicator_id = "idmc_displacement_disaster",
  alert_fn = alert_displacement$alert,
  plot_fn = plot_conflict$plot,
  info_fn = NULL,
  first_run = first_run
)
