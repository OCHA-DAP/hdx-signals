box::use(./utils/raw_agricultural_hotspots)
box::use(./utils/wrangle_agricultural_hotspots)
box::use(./utils/alert_agricultural_hotspots)
box::use(./utils/summary_agricultural_hotspots)
box::use(./utils/info_agricultural_hotspots)
box::use(./utils/plot_agricultural_hotspots)

box::use(../../alerts/generate_signals[generate_signals])

box::use(logger[log_info])

test <- as.logical(Sys.getenv("TEST", unset = FALSE))
test_filter <- if (test) c("AFG", "SSD") else NULL

log_info("Checking agricultural hotspots indicator... Running with:")
log_info(paste0("GMAS_TEST_RUN = ", Sys.getenv("GMAS_TEST_RUN")))
log_info(paste0("TEST = ", Sys.getenv("TEST")))
log_info(paste0("FIRST_RUN = ", Sys.getenv("FIRST_RUN")))

df_raw <- raw_agricultural_hotspots$raw()
df_wrangled <- wrangle_agricultural_hotspots$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "jrc_agricultural_hotspots",
  alert_fn = alert_agricultural_hotspots$alert,
  summary_fn = summary_agricultural_hotspots$summary,
  info_fn = info_agricultural_hotspots$info,
  plot_fn = plot_agricultural_hotspots$plot,
  test = test, 
  test_filter = test_filter
)

log_info("Successfully checked agricultural hotspots indicator")
