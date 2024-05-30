box::use(assertthat)

box::use(../../src/indicators/jrc_agricultural_hotspots/utils/wrangle_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/alert_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/summary_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/info_agricultural_hotspots)
box::use(../../src/indicators/jrc_agricultural_hotspots/utils/plot_agricultural_hotspots)

box::use(../../src/alerts/generate_signals[generate_signals])
box::use(../../src/utils/hs_logger)
hs_logger$configure_logger()

# Create a simple wrapper function so that we can just pass in the wrangled df
generate_signals_wrapper <- function(df_wrangled) {
  generate_signals(
    df_wrangled = df_wrangled,
    indicator_id = "jrc_agricultural_hotspots",
    alert_fn = alert_agricultural_hotspots$alert,
    summary_fn = summary_agricultural_hotspots$summary,
    info_fn = info_agricultural_hotspots$info,
    plot_fn = plot_agricultural_hotspots$plot,
    dry_run = dry_run,
    dry_run_filter = dry_run_filter
  )
}

# Test basic case
df_jrc <- readRDS("test/data/jrc_hotspots_basic.RDS") |>
  wrangle_agricultural_hotspots$wrangle() |>
  generate_signals_wrapper()

assertthat$assert_that(nrow(df_jrc) == 0, msg = "Output DataFrame should not have any signals")
assertthat$assert_that(ncol(df_jrc) == 38, msg = "Output DataFrame does not have the correct number of columns")

# TODO: Create additional test cases with different input dfs
