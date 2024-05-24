box::use(dplyr)
box::use(logger[log_info, log_debug])

# indicator utilities
box::use(./utils/raw_displacement)
box::use(./utils/wrangle_displacement)
box::use(./utils/alert_displacement)
box::use(./utils/plot_displacement)
box::use(./utils/info_displacement)
box::use(./utils/summary_displacement)
box::use(./utils/map_displacement)

box::use(../../alerts/generate_signals[generate_signals])
box::use(../../utils/hs_logger)

test <- as.logical(Sys.getenv("HS_TEST", unset = FALSE))
test_filter <- if (test) c("AFG", "SSD") else NULL
indicator_id <- "idmc_displacement_conflict"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

df_raw <- raw_displacement$raw()
df_wrangled <- wrangle_displacement$wrangle(df_raw)

# now generate signals individually for each displacement type
df_conflict <- generate_signals(
  df_wrangled = dplyr$filter(df_wrangled, displacement_type == "Conflict"),
  df_raw = dplyr$filter(df_raw, displacement_type == "Conflict"),
  indicator_id = indicator_id,
  alert_fn = alert_displacement$alert,
  plot_fn = plot_displacement$plot,
  info_fn = info_displacement$info,
  summary_fn = summary_displacement$summary,
  map_fn = map_displacement$map,
  test = test,
  test_filter = test_filter
)

df_disaster <- generate_signals(
  df_wrangled = dplyr$filter(df_wrangled, displacement_type == "Disaster"),
  df_raw = dplyr$filter(df_raw, displacement_type == "Disaster"),
  indicator_id = indicator_id,
  alert_fn = alert_displacement$alert,
  plot_fn = plot_displacement$plot,
  info_fn = info_displacement$info,
  summary_fn = summary_displacement$summary,
  map_fn = map_displacement$map,
  test = test,
  test_filter = test_filter
)
