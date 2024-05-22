box::use(./utils/raw_food_insecurity)
box::use(./utils/wrangle_food_insecurity)
box::use(./utils/alert_food_insecurity)
box::use(./utils/plot_food_insecurity)
box::use(./utils/summary_food_insecurity)
box::use(./utils/map_food_insecurity)
box::use(./utils/info_food_insecurity)

box::use(../../alerts/generate_signals[generate_signals])

box::use(logger[log_info])

test <- as.logical(Sys.getenv("TEST", unset = FALSE))
test_filter <- if (test) c("AFG", "SSD") else NULL

log_info("Checking food insecurity indicator... Running with:")
log_info(paste0("GMAS_TEST_RUN = ", Sys.getenv("GMAS_TEST_RUN")))
log_info(paste0("TEST = ", Sys.getenv("TEST")))
log_info(paste0("FIRST_RUN = ", Sys.getenv("FIRST_RUN")))

df_raw <- raw_food_insecurity$raw()
df_wrangled <- wrangle_food_insecurity$wrangle(df_raw)

# now generate signals
df_ipc <- generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "ipc_food_insecurity",
  alert_fn = alert_food_insecurity$alert,
  plot_fn = plot_food_insecurity$plot,
  summary_fn = summary_food_insecurity$summary,
  map_fn = map_food_insecurity$map,
  info_fn = info_food_insecurity$info,
  test = test,
  test_filter = test_filter
)

log_info("Successfully checked food insecurity indicator")
