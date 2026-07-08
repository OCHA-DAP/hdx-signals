box::use(
  dplyr,
  gg = ggplot2,
  rnaturalearth
)

box::use(
  src/images/create_images,
  src/images/plots/caption,
  src/images/maps/map_theme
)

# Thresholds matching ds-seas5-skill defaults.
# vsev/sev percentile bounds are derived from return periods.
SEA5_R_MOD   <- 0.3
SEA5_R_HIGH  <- 0.5
SEA5_VSEV_M  <- 100 / 10  # 10th / 90th percentile (10-year RP)
SEA5_SEV_M   <- 100 / 3   # 33rd / 67th percentile (3-year RP)

# Ordered factor levels — determines legend order.
CATEGORY_LEVELS <- c(
  "strongly_below", "below_normal", "roughly_normal",
  "above_normal", "strongly_above",
  "roughly_normal_mod", "low_skill", "off_season", "not_monitored"
)

CATEGORY_COLORS <- c(
  "strongly_below"     = "#7B3A1A",
  "below_normal"       = "#C8844A",
  "roughly_normal"     = "#FFFFFF",
  "above_normal"       = "#71B3E5",
  "strongly_above"     = "#0D40B0",
  "roughly_normal_mod" = "#E0E0E0",
  "low_skill"          = "#C8C8C8",
  "off_season"         = "#D0D0D0",
  "not_monitored"      = "#F5F5F5"
)

CATEGORY_LABELS <- c(
  "strongly_below"     = "Strongly below normal",
  "below_normal"       = "Below normal",
  "roughly_normal"     = "Roughly normal",
  "above_normal"       = "Above normal",
  "strongly_above"     = "Strongly above normal",
  "roughly_normal_mod" = "Roughly normal (mod skill)",
  "low_skill"          = "Low skill",
  "off_season"         = "Outside rainy season",
  "not_monitored"      = "Not monitored"
)

#' Map SEA5 anomaly
#'
#' Creates a global choropleth map for SEA5 anomaly, showing forecast categories
#' for the next valid trimester from the forecast date across all countries.
#' Categories and colours match the ds-seas5-skill reference map.
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw Raw data frame. Must contain columns `iso3`, `date`,
#'     `forecast_percentile`, `pearson_r`, and `is_rainy`.
#' @param preview Whether or not to preview the plots
#'
#' @export
map <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  create_images$create_images(
    df_alerts = df_alerts,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = sea5_anomaly_map,
    image_use = "map",
    width = 10,
    height = 5,
    settings = "plot"
  )
}

#' Map SEA5 anomaly data at global level
#'
#' Produces a global choropleth map with countries coloured by their forecast
#' category for the next valid trimester. Category logic is a faithful port of
#' the ds-seas5-skill `classify()` function (docs/app.js).
#'
#' @param df_wrangled Wrangled data frame for plotting (single alerted country).
#' @param df_raw Raw data frame (all countries).
#' @param title Plot title (unused; title is fixed to reflect the trimester).
#' @param date Date of the alert.
#'
#' @returns Global choropleth ggplot object
sea5_anomaly_map <- function(df_wrangled, df_raw, title, date) {
  # compute next valid trimester label (starts 1 month after forecast date)
  forecast_month <- as.integer(format(date, "%m"))
  start_month <- (forecast_month %% 12L) + 1L
  trimester_month_nums <- (start_month - 1L + c(0L, 1L, 2L)) %% 12L + 1L
  month_initials <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
  trimester_label <- paste(month_initials[trimester_month_nums], collapse = "")

  # filter to the most recent forecast date up to the alert date, then classify
  alert_date <- date
  df_map_data <- df_raw |>
    dplyr$filter(date <= alert_date) |>
    dplyr$filter(date == max(date)) |>
    dplyr$transmute(
      iso3,
      category = dplyr$case_when(
        is.na(pearson_r) | is.na(forecast_percentile) ~ "off_season",
        !is_rainy                                      ~ "off_season",
        pearson_r < SEA5_R_MOD                         ~ "low_skill",
        # strongly below / above normal (outside 10th / 90th pct)
        forecast_percentile <= SEA5_VSEV_M &
          forecast_percentile < 50                     ~ "strongly_below",
        forecast_percentile >= 100 - SEA5_VSEV_M &
          forecast_percentile >= 50                    ~ "strongly_above",
        # below / above normal (10th–33rd / 67th–90th pct)
        forecast_percentile > SEA5_VSEV_M &
          forecast_percentile <= SEA5_SEV_M            ~ "below_normal",
        forecast_percentile >= 100 - SEA5_SEV_M &
          forecast_percentile < 100 - SEA5_VSEV_M      ~ "above_normal",
        # roughly normal: split by skill level
        pearson_r >= SEA5_R_HIGH                       ~ "roughly_normal",
        .default                                        = "roughly_normal_mod"
      )
    )

  sf_world <- rnaturalearth$ne_countries(scale = "medium", returnclass = "sf")

  # join and assign "not_monitored" to any country absent from df_raw
  sf_data <- sf_world |>
    dplyr$left_join(df_map_data, by = c("iso_a3" = "iso3")) |>
    dplyr$mutate(
      category = dplyr$if_else(is.na(category), "not_monitored", category),
      category = factor(category, levels = CATEGORY_LEVELS)
    )

  map_caption <- caption$caption(
    indicator_id = "sea5_anomaly",
    iso3 = unique(df_wrangled$iso3),
    map = FALSE
  )

  gg$ggplot() +
    gg$geom_sf(
      data = sf_data,
      mapping = gg$aes(fill = category),
      color = "#5A5A5A",
      linewidth = 0.1
    ) +
    gg$scale_fill_manual(
      values = CATEGORY_COLORS,
      labels = CATEGORY_LABELS,
      drop = FALSE,
      name = NULL
    ) +
    gg$coord_sf(
      clip = "off",
      crs = "OGC:CRS84"
    ) +
    gg$labs(
      x = "",
      y = "",
      title = "SEA5 seasonal forecast",
      subtitle = trimester_label,
      caption = map_caption
    ) +
    map_theme$map_theme(
      iso3 = unique(df_wrangled$iso3),
      use_map_settings = FALSE,
      margin_location = "subtitle"
    )
}
