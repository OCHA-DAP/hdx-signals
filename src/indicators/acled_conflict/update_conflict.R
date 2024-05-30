box::use(logger[log_info, log_debug])

# indicator utilities
box::use(./utils/raw_conflict)
box::use(./utils/wrangle_conflict)
box::use(./utils/alert_conflict)
box::use(./utils/plot_conflict)
box::use(./utils/map_conflict)
box::use(./utils/info_conflict)
box::use(./utils/summary_conflict)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../utils/hs_logger)

dry_run <- as.logical(Sys.getenv("HS_DRY_RUN", unset = TRUE))
dry_run_filter <- if (dry_run) c("AFG", "SSD") else NULL
indicator_id <- "acled_conflict"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

# get data
df_raw <- raw_conflict$raw()
df_wrangled <- wrangle_conflict$wrangle(df_raw)

# now generate signals
df_conflict <- generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = indicator_id,
  alert_fn = alert_conflict$alert,
  plot_fn = plot_conflict$plot,
  info_fn = info_conflict$info,
  map_fn = map_conflict$map,
  summary_fn = summary_conflict$summary,
  dry_run = dry_run,
  dry_run_filter = dry_run_filter
)
