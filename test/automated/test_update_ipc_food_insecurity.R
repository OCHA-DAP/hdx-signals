box::use(assertthat)

box::use(../../src/indicators/ipc_food_insecurity/utils/wrangle_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/alert_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/plot_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/summary_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/map_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/info_food_insecurity)

box::use(../../src/alerts/generate_signals[generate_signals])
box::use(../../src/utils/hs_logger)
hs_logger$configure_logger()

# Create a simple wrapper function so that we can just pass in the wrangled df
generate_signals_wrapper <- function(df_wrangled) {
  generate_signals(
    df_wrangled = df_wrangled,
    indicator_id = "ipc_food_insecurity",
    alert_fn = alert_food_insecurity$alert,
    plot_fn = plot_food_insecurity$plot,
    summary_fn = summary_food_insecurity$summary,
    map_fn = map_food_insecurity$map,
    info_fn = info_food_insecurity$info,
    dry_run = FALSE,
    dry_run_filter = NULL
  )
}

# Test basic case
df_ipc <- readRDS("test/data/ipc_basic.RDS") |>
  wrangle_food_insecurity$wrangle() |>
  generate_signals_wrapper()

assertthat$assert_that(nrow(df_ipc) == 0, msg = "Output DataFrame should not have any signals")
assertthat$assert_that(ncol(df_ipc) == 38, msg = "Output DataFrame does not have the correct number of columns")

# TODO: Create additional test cases with different input dfs
