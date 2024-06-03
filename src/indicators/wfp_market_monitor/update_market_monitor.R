# indicator utilities
box::use(./utils/raw_market_monitor)
box::use(./utils/wrangle_market_monitor)
box::use(./utils/alert_market_monitor)
box::use(./utils/plot_market_monitor)
box::use(./utils/info_market_monitor)
box::use(./utils/summary_market_monitor)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../utils/hs_logger)

test <- as.logical(Sys.getenv("HS_TEST", unset = TRUE))
test_filter <- if (test) c("BDI", "SSD") else NULL
indicator_id <- "wfp_market_monitor"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

# get data
df_raw <- raw_market_monitor$raw()
df_wrangled <- wrangle_market_monitor$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = "wfp_market_monitor",
  alert_fn = alert_market_monitor$alert,
  plot_fn = plot_market_monitor$plot,
  info_fn = info_market_monitor$info,
  summary_fn = summary_market_monitor$summary
)
