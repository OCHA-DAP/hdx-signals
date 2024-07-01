box::use(
  src/indicators/acled_conflict/utils/raw_conflict,
  src/indicators/acled_conflict/utils/wrangle_conflict,
  src/indicators/acled_conflict/utils/alert_conflict,
  src/indicators/acled_conflict/utils/plot_conflict,
  src/indicators/acled_conflict/utils/map_conflict,
  src/indicators/acled_conflict/utils/info_conflict,
  src/indicators/acled_conflict/utils/summary_conflict,
  src/signals/generate_signals,
  src/utils/hs_logger,
  src/utils/update_coverage
)

first_run <- as.logical(Sys.getenv("FIRST_RUN", unset = FALSE))
dry_run <- as.logical(Sys.getenv("HS_DRY_RUN", unset = TRUE))
dry_run_filter <- if (dry_run) c("AFG", "SSD") else NULL
indicator_id <- "acled_conflict"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

# get data
df_raw <- raw_conflict$raw()
df_wrangled <- wrangle_conflict$wrangle(df_raw)

# update coverage data
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

# now generate signals
df_conflict <- generate_signals$generate_signals( # nolint
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = indicator_id,
  alert_fn = alert_conflict$alert,
  plot_fn = plot_conflict$plot,
  info_fn = info_conflict$info,
  map_fn = map_conflict$map,
  summary_fn = summary_conflict$summary,
  dry_run = dry_run,
  dry_run_filter = dry_run_filter,
  first_run = first_run
)
