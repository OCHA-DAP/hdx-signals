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

logger$configure_logger()

test <- as.logical(Sys.getenv("TEST", unset = FALSE))
test_filter <- if (test) c("AFG", "SSD") else NULL

log_info("Checking acled conflict indicator...")
log_debug(paste0("GMAS_TEST_RUN = ", Sys.getenv("GMAS_TEST_RUN")))
log_debug(paste0("TEST = ", Sys.getenv("TEST")))
log_debug(paste0("FIRST_RUN = ", Sys.getenv("FIRST_RUN")))

# get data
df_raw <- raw_conflict$raw()
df_wrangled <- wrangle_conflict$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = "acled_conflict",
  alert_fn = alert_conflict$alert,
  plot_fn = plot_conflict$plot,
  info_fn = info_conflict$info,
  map_fn = map_conflict$map,
  summary_fn = summary_conflict$summary,
  test = test,
  test_filter = test_filter
)

log_info("Successfully checked acled conflict indicator")
