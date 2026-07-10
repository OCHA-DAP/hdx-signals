box::use(
  src/indicators/idmc_displacement/utils/raw_displacement,
  src/indicators/idmc_displacement/utils/wrangle_displacement,
  src/indicators/acled_conflict/utils/raw_conflict,
  src/indicators/acled_conflict/utils/wrangle_conflict,
  src/indicators/jrc_agricultural_hotspots/utils/raw_agricultural_hotspots,
  src/indicators/jrc_agricultural_hotspots/utils/wrangle_agricultural_hotspots,
  src/indicators/acaps_inform_severity/utils/raw_inform,
  src/indicators/acaps_inform_severity/utils/wrangle_inform,
  src/indicators/ipc_food_insecurity/utils/raw_food_insecurity,
  src/indicators/ipc_food_insecurity/utils/wrangle_food_insecurity,
  src/indicators/wfp_market_monitor/utils/raw_market_monitor,
  src/indicators/wfp_market_monitor/utils/wrangle_market_monitor,
  src/indicators/who_cholera/utils/raw_cholera,
  src/indicators/who_cholera/utils/wrangle_cholera,
  src/indicators/sea5_anomaly/utils/raw_sea5_anomaly,
  src/indicators/sea5_anomaly/utils/wrangle_sea5_anomaly,
  src/utils/location_codes
)

box::use(
  dplyr,
  logger,
  readr
)

# Each indicator's raw/wrangle pair. `wrangle()` is what standardizes the
# country identifier to an `iso3` column across all indicators.
indicators <- list(
  idmc_displacement = list(raw = raw_displacement$raw, wrangle = wrangle_displacement$wrangle),
  acled_conflict = list(raw = raw_conflict$raw, wrangle = wrangle_conflict$wrangle),
  jrc_agricultural_hotspots = list(
    raw = raw_agricultural_hotspots$raw,
    wrangle = wrangle_agricultural_hotspots$wrangle
  ),
  acaps_inform_severity = list(raw = raw_inform$raw, wrangle = wrangle_inform$wrangle),
  ipc_food_insecurity = list(raw = raw_food_insecurity$raw, wrangle = wrangle_food_insecurity$wrangle),
  wfp_market_monitor = list(raw = raw_market_monitor$raw, wrangle = wrangle_market_monitor$wrangle),
  who_cholera = list(raw = raw_cholera$raw, wrangle = wrangle_cholera$wrangle),
  sea5_anomaly = list(raw = raw_sea5_anomaly$raw, wrangle = wrangle_sea5_anomaly$wrangle)
)

#' Get unique ISO3 country codes monitored by a single indicator
#'
#' @param indicator_name Name of the indicator, used for logging
#' @param raw_fn Function that downloads the indicator's raw data
#' @param wrangle_fn Function that wrangles the raw data into standard format
#'
#' @returns Character vector of unique ISO3 codes
get_indicator_countries <- function(indicator_name, raw_fn, wrangle_fn) {
  logger$log_info("Fetching raw data for {indicator_name}")
  df_raw <- raw_fn()
  df_wrangled <- wrangle_fn(df_raw)
  unique(df_wrangled$iso3)
}

# Fetch and wrangle each indicator, tracking which countries each one covers
iso3_by_indicator <- Map(
  \(indicator_name, funs) get_indicator_countries(indicator_name, funs$raw, funs$wrangle),
  names(indicators),
  indicators
)

# Build a country -> number of indicators covering it lookup
df_countries <- dplyr$tibble(
  iso3 = unlist(iso3_by_indicator),
  indicator_name = rep(names(iso3_by_indicator), lengths(iso3_by_indicator))
) |>
  dplyr$filter(!is.na(iso3) & iso3 != "") |>
  dplyr$distinct(iso3, indicator_name) |>
  dplyr$group_by(iso3) |>
  dplyr$summarise(n_indicators = dplyr$n(), .groups = "drop") |>
  dplyr$mutate(location = location_codes$iso3_to_names(iso3)) |>
  dplyr$select(iso3, location, n_indicators) |>
  dplyr$arrange(iso3)

n_total_countries <- nrow(df_countries)

logger$log_info(
  "Total unique countries monitored across all indicators: {n_total_countries}"
)

# Save the country list to CSV; the total count is logged above
out_dir <- "adhoc/country_counts"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

gen_date <- format(Sys.Date(), "%Y-%m-%d")
out_file <- file.path(out_dir, paste0("unique_countries_", gen_date, ".csv"))

readr$write_csv(df_countries, out_file)

logger$log_info("Saved unique country list to {out_file}")
