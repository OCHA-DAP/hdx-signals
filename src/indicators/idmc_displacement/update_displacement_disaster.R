box::use(dplyr)

# indicator utilities
box::use(./utils/raw_displacement)
box::use(./utils/wrangle_displacement)
box::use(./utils/alert_displacement)
box::use(./utils/plot_displacement)
box::use(./utils/info_displacement)
box::use(./utils/summary_displacement)
box::use(./utils/map_displacement)

box::use(../../signals/generate_signals)
box::use(../../utils/hs_logger)
box::use(../../utils/update_coverage)

dry_run_filter <- c("AFG", "SSD")
indicator_id <- "idmc_displacement_disaster"

hs_logger$monitoring_log_setup(indicator_id)

df_raw <- raw_displacement$raw() |>
  dplyr$filter(displacement_type == "Disaster")

df_wrangled <- wrangle_displacement$wrangle(df_raw)

# update coverage data to ensure locations_metadata up to date
update_coverage$update_coverage(
  indicator_id = indicator_id,
  iso3 = df_wrangled$iso3
)

df_disaster <- generate_signals$generate_signals(
  df_wrangled = df_wrangled,
  df_raw = df_raw,
  indicator_id = indicator_id,
  alert_fn = alert_displacement$alert,
  plot_fn = plot_displacement$plot,
  info_fn = info_displacement$info,
  summary_fn = summary_displacement$summary,
  map_fn = map_displacement$map,
  dry_run_filter = dry_run_filter
)
