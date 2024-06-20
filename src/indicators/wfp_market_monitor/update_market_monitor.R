# indicator utilities
box::use(./utils/raw_market_monitor)
box::use(./utils/wrangle_market_monitor)
box::use(./utils/alert_market_monitor)
box::use(./utils/plot_market_monitor)
box::use(./utils/info_market_monitor)
box::use(./utils/summary_market_monitor)

box::use(../../signals/generate_signals)
box::use(../../utils/hs_logger)
box::use(../../utils/update_coverage)

first_run <- as.logical(Sys.getenv("FIRST_RUN", unset = FALSE))
dry_run <- as.logical(Sys.getenv("HS_DRY_RUN", unset = TRUE))
dry_run_filter <- if (dry_run) c("BDI", "SSD") else NULL
indicator_id <- "wfp_market_monitor"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

# get data
df_raw <- raw_market_monitor$raw()
df_wrangled <- wrangle_market_monitor$wrangle(df_raw)

# update coverage for ipc
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

# now generate signals
df_market_monitor <- generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = "wfp_market_monitor",
  alert_fn = alert_market_monitor$alert,
  plot_fn = plot_market_monitor$plot,
  info_fn = info_market_monitor$info,
  summary_fn = summary_market_monitor$summary,
  dry_run = dry_run,
  dry_run_filter = dry_run_filter,
  first_run = first_run
)
