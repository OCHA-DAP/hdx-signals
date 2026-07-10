box::use(
  dplyr,
  gg = ggplot2,
  ggpattern,
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

# Ordered factor levels — determines legend order (drier to wetter, then extras).
ANOMALY_LEVELS <- c(
  "strongly_below", "below_normal", "roughly_normal",
  "above_normal", "strongly_above",
  "off_season", "not_monitored"
)

# Brand color tokens, one contiguous strip from drier to wetter.
ANOMALY_COLORS <- c(
  "strongly_below" = "#7f5619",
  "below_normal"   = "#dda555",
  "roughly_normal" = "#e2e8e8",
  "above_normal"   = "#74a1e8",
  "strongly_above" = "#134ead",
  "off_season"     = "#b1c1c2",
  "not_monitored"  = "#f5f7f7"
)

ANOMALY_LABELS <- c(
  "strongly_below" = "Strongly below normal",
  "below_normal"   = "Below normal",
  "roughly_normal" = "Roughly normal",
  "above_normal"   = "Above normal",
  "strongly_above" = "Strongly above normal",
  "off_season"     = "Outside rainy season",
  "not_monitored"  = "Not monitored"
)

# Skill strip, shown as a solid/hatched pattern overlaid on the anomaly color.
# "not_applicable" covers off-season / unmonitored countries — mapped to a
# solid pattern but excluded from the legend via `breaks` (below).
SKILL_LEVELS <- c("high_skill", "moderate_skill", "low_skill", "not_applicable")
SKILL_BREAKS <- c("high_skill", "moderate_skill", "low_skill")

SKILL_PATTERNS <- c(
  "high_skill"     = "none",
  "moderate_skill" = "stripe",
  "low_skill"      = "crosshatch",
  "not_applicable" = "none"
)

SKILL_LABELS <- c(
  "high_skill"     = "High skill",
  "moderate_skill" = "Moderate skill",
  "low_skill"      = "Low skill"
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
#' anomaly category for the next valid trimester (one contiguous strip from
#' drier to wetter), with forecast skill shown as a solid/hatched pattern
#' overlaid on top of the anomaly colour.
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
        is.na(forecast_percentile) | is.na(is_rainy) | !is_rainy ~ "off_season",
        forecast_percentile <= SEA5_VSEV_M          ~ "strongly_below",
        forecast_percentile <= SEA5_SEV_M           ~ "below_normal",
        forecast_percentile < 100 - SEA5_SEV_M      ~ "roughly_normal",
        forecast_percentile < 100 - SEA5_VSEV_M     ~ "above_normal",
        .default                                     = "strongly_above"
      ),
      skill = dplyr$case_when(
        category == "off_season" | is.na(pearson_r) ~ "not_applicable",
        pearson_r >= SEA5_R_HIGH                    ~ "high_skill",
        pearson_r >= SEA5_R_MOD                     ~ "moderate_skill",
        .default                                    = "low_skill"
      )
    )

  sf_world <- rnaturalearth$ne_countries(scale = "medium", returnclass = "sf")

  # join and assign "not_monitored" to any country absent from df_raw
  sf_data <- sf_world |>
    dplyr$left_join(df_map_data, by = c("iso_a3" = "iso3")) |>
    dplyr$mutate(
      category = dplyr$if_else(is.na(category), "not_monitored", category),
      category = factor(category, levels = ANOMALY_LEVELS),
      skill = dplyr$if_else(is.na(skill), "not_applicable", skill),
      skill = factor(skill, levels = SKILL_LEVELS)
    )

  map_caption <- caption$caption(
    indicator_id = "sea5_anomaly",
    iso3 = unique(df_wrangled$iso3),
    map = FALSE
  )

  gg$ggplot() +
    ggpattern$geom_sf_pattern(
      data = sf_data,
      mapping = gg$aes(fill = category, pattern = skill),
      color = "#5A5A5A",
      linewidth = 0.1,
      pattern_fill = "#5A5A5A",
      pattern_colour = NA,
      pattern_density = 0.15,
      pattern_spacing = 0.015,
      pattern_size = 0.15
    ) +
    gg$scale_fill_manual(
      values = ANOMALY_COLORS,
      labels = ANOMALY_LABELS,
      drop = FALSE,
      name = NULL
    ) +
    ggpattern$scale_pattern_manual(
      values = SKILL_PATTERNS,
      breaks = SKILL_BREAKS,
      labels = SKILL_LABELS,
      drop = FALSE,
      name = "Forecast skill"
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
    gg$guides(
      fill = gg$guide_legend(order = 1, override.aes = list(pattern = "none")),
      pattern = gg$guide_legend(order = 2, override.aes = list(fill = "#e2e8e8"))
    ) +
    map_theme$map_theme(
      iso3 = unique(df_wrangled$iso3),
      use_map_settings = FALSE,
      margin_location = "subtitle"
    )
}
