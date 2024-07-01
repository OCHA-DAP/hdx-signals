box::use(dplyr)

box::use(
  src/indicators/idmc_displacement/utils/raw_displacement,
  src/indicators/idmc_displacement/utils/wrangle_displacement,
  src/indicators/idmc_displacement/utils/alert_displacement,
  src/indicators/idmc_displacement/utils/plot_displacement,
  src/indicators/idmc_displacement/utils/info_displacement,
  src/indicators/idmc_displacement/utils/summary_displacement,
  src/indicators/idmc_displacement/utils/map_displacement,
  src/signals/generate_signals,
  src/utils/hs_logger,
  src/utils/update_coverage
)

first_run <- as.logical(Sys.getenv("FIRST_RUN", unset = FALSE))
dry_run <- as.logical(Sys.getenv("HS_DRY_RUN", unset = TRUE))
dry_run_filter <- if (dry_run) c("AFG", "SSD") else NULL
indicator_id <- "idmc_displacement_conflict"

hs_logger$configure_logger()
hs_logger$monitoring_log_setup(indicator_id)

df_raw <- raw_displacement$raw() |>
  dplyr$filter(
    displacement_type == "Conflict"
  )
df_wrangled <- wrangle_displacement$wrangle(df_raw)

# update coverage data to ensure locations_metadata up to date
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = indicator_id,
  alert_fn = alert_displacement$alert,
  plot_fn = plot_displacement$plot,
  info_fn = info_displacement$info,
  summary_fn = summary_displacement$summary,
  map_fn = map_displacement$map,
  dry_run = dry_run,
  dry_run_filter = dry_run_filter,
  first_run = first_run
)
