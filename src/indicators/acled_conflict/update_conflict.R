box::use(./utils/raw_conflict)
box::use(./utils/wrangle_conflict)
box::use(./utils/alert_conflict)
box::use(./utils/plot_conflict)
box::use(./utils/info_conflict)
box::use(./utils/summary_conflict)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../alerts/generate_alerts[generate_alerts])
box::use(../../alerts/triage_signals)
box::use(../../alerts/delete_campaign_content)

Sys.setenv(GMAS_TEST_RUN = FALSE)

first_run <- TRUE

df_raw <- raw_conflict$raw(first_run)
df_wrangled <- wrangle_conflict$wrangle(df_raw, first_run)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "acled_conflict",
  alert_fn = alert_conflict$alert,
  plot_fn = plot_conflict$plot,
  info_fn = info_conflict$info,
  summary_fn = NULL,
  first_run = first_run
)
