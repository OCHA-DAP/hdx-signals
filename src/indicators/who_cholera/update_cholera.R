box::use(./utils/raw_cholera)
box::use(./utils/wrangle_cholera)
box::use(./utils/alert_cholera)
box::use(./utils/summary_cholera)
box::use(./utils/plot_cholera)
box::use(./utils/info_cholera)

box::use(../../signals/generate_signals)

first_run <- as.logical(Sys.getenv("FIRST_RUN", unset = FALSE))
dry_run <- as.logical(Sys.getenv("HS_DRY_RUN", unset = TRUE))
dry_run_filter <- if (dry_run) c("ETH", "SSD") else NULL
indicator_id <- "who_cholera"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

df_wrangled <- raw_cholera$raw() |>
  wrangle_cholera$wrangle()

# now generate signals
generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = indicator_id,
  alert_fn = alert_cholera$alert,
  plot_fn = plot_cholera$plot,
  summary_fn = summary_cholera$summary,
  info_fn = info_cholera$info,
  first_run = first_run,
  dry_run = dry_run,
  dry_run_filter = dry_run_filter
)
