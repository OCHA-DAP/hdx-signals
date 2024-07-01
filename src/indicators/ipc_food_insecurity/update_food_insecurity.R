box::use(
  src/indicators/ipc_food_insecurity/utils/raw_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/wrangle_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/alert_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/plot_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/summary_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/map_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/info_food_insecurity,
  src/signals/generate_signals,
  src/utils/hs_logger,
  src/utils/update_coverage
)

first_run <- as.logical(Sys.getenv("FIRST_RUN", unset = FALSE))
dry_run <- as.logical(Sys.getenv("HS_DRY_RUN", unset = TRUE))
dry_run_filter <- if (dry_run) c("AFG", "SSD") else NULL
indicator_id <- "ipc_food_insecurity"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

df_raw <- raw_food_insecurity$raw()
df_wrangled <- wrangle_food_insecurity$wrangle(df_raw)

# update coverage for ipc
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

# now generate signals
generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = indicator_id,
  alert_fn = alert_food_insecurity$alert,
  plot_fn = plot_food_insecurity$plot,
  summary_fn = summary_food_insecurity$summary,
  map_fn = map_food_insecurity$map,
  info_fn = info_food_insecurity$info,
  dry_run = dry_run,
  dry_run_filter = dry_run_filter,
  first_run = first_run
)
