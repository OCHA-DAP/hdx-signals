box::use(testthat)
box::use(httptest)

box::use(../../src/indicators/ipc_food_insecurity/utils/wrangle_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/alert_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/plot_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/summary_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/map_food_insecurity)
box::use(../../src/indicators/ipc_food_insecurity/utils/info_food_insecurity)
box::use(../../src/alerts/generate_signals[generate_signals])

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

# TODO: Work on wrapping with httptest$with_mock_dir("test/mock_apis", {)
# to mock API requests to IPC API
options(httptest.verbose = TRUE)

testthat$test_that("the base case for IPC food insecurity runs and doesn't create any signals", {
  df_ipc <- readRDS("test/data/ipc_basic.RDS") |>
    wrangle_food_insecurity$wrangle() |>
    generate_signals_wrapper()
  testthat$expect_equal(nrow(df_ipc), 0)
  testthat$expect_equal(ncol(df_ipc), 38)
})
