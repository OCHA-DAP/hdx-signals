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

indicator_id <- "acled_conflict"
hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

# TODO: Load in some dummy raw data
# df_raw <- create_dummy_acled_raw()
df_wrangled <- wrangle_conflict$wrangle(df_raw)

# test should be false since we want to mimic "production"
# behaviour as much as possible. But should be run with
# GMAS_TEST_RUN = FALSE so that we don't connect to any external systems
df_conflict <- generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = indicator_id,
  alert_fn = alert_conflict$alert,
  plot_fn = plot_conflict$plot,
  info_fn = info_conflict$info,
  map_fn = map_conflict$map,
  summary_fn = summary_conflict$summary,
  test = FALSE
)

# TODO: Check output is what we expect
# assert len df_conflict == some number
# assert [AFG, SSD] in df_conflict$iso3
