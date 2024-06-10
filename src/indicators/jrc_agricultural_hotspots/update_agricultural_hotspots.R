box::use(./utils/raw_agricultural_hotspots)
box::use(./utils/wrangle_agricultural_hotspots)
box::use(./utils/alert_agricultural_hotspots)
box::use(./utils/summary_agricultural_hotspots)
box::use(./utils/info_agricultural_hotspots)
box::use(./utils/plot_agricultural_hotspots)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../utils/hs_logger)
box::use(../../utils/update_coverage)

dry_run <- as.logical(Sys.getenv("HS_DRY_RUN", unset = TRUE))
dry_run_filter <- if (dry_run) c("AFG", "SSD") else NULL
indicator_id <- "jrc_agricultural_hotspots"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

df_raw <- raw_agricultural_hotspots$raw()
df_wrangled <- wrangle_agricultural_hotspots$wrangle(df_raw)

# update coverage data to ensure locations_metadata up to date
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

# now generate signals
df_jrc <- generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = indicator_id,
  alert_fn = alert_agricultural_hotspots$alert,
  summary_fn = summary_agricultural_hotspots$summary,
  info_fn = info_agricultural_hotspots$info,
  plot_fn = plot_agricultural_hotspots$plot,
  dry_run = dry_run,
  dry_run_filter = dry_run_filter
)
