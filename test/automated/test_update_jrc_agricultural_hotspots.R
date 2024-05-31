box::use(testthat)
box::use(httptest)

box::use(../../src/email/mailchimp/base_api)
box::use(../../src/alerts/generate_signals[generate_signals])
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/wrangle_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/alert_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/summary_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/info_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/plot_agricultural_hotspots)

# Create a simple wrapper function so that we can just pass in the wrangled df
generate_signals_wrapper <- function(df_wrangled) {
  generate_signals(
    df_wrangled = df_wrangled,
    indicator_id = "jrc_agricultural_hotspots",
    alert_fn = alert_agricultural_hotspots$alert,
    summary_fn = summary_agricultural_hotspots$summary,
    info_fn = info_agricultural_hotspots$info,
    plot_fn = plot_agricultural_hotspots$plot,
    dry_run = FALSE,
    dry_run_filter = NULL
  )
}

testthat$test_that("the base case for JRC hotspots runs and doesn't create any signals", {
  df_jrc <- readRDS("test/data/jrc_hotspots_basic.RDS") |>
    wrangle_agricultural_hotspots$wrangle() |>
    generate_signals_wrapper()
  testthat$expect_equal(nrow(df_jrc), 0)
  testthat$expect_equal(ncol(df_jrc), 38)
})
