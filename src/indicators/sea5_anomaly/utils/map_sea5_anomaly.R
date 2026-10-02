box::use(
  dplyr,
  gg = ggplot2,
  ggpattern,
  glue,
  logger,
  stats
)

box::use(
  src/images/create_images,
  src/images/plots/caption,
  src/images/plots/hdx_signals_palette,
  src/images/maps/sf_adm0,
  src/images/maps/map_theme,
  src/indicators/sea5_anomaly/utils/alert_sea5_anomaly,
  src/indicators/sea5_anomaly/utils/palette_sea5_anomaly,
  src/utils/download_shapefile,
  src/utils/iso3_shift_longitude
)

# Return period classes for alerting units come from `palette_sea5_anomaly`,
# light to dark within each direction. In-season units that do not qualify
# (return period below the threshold or low hindcast skill) and off-season
# units get their own neutral classes so every unit on the map is accounted for
# in the legend. Off-season units are striped.
# wrapped so the legend stays narrow enough for the small map canvases
not_alerting_label <- paste0("RP under ", alert_sea5_anomaly$seas5_rp_years, "\nor low skill")
off_season_label <- "Off season"
class_levels <- c(palette_sea5_anomaly$rp_labels, not_alerting_label, off_season_label)
class_patterns <- stats$setNames(c(rep("none", 4), "stripe"), class_levels)

#' Map SEA5 anomaly
#'
#' Maps the admin 1 units alerting for the country's firing trimester, coloured
#' by the return period of the forecast anomaly in the signal direction.
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw Raw data frame
#' @param preview Whether or not to preview the plots
#'
#' @export
map <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  df_map <- df_alerts |>
    dplyr$mutate(
      # two lines so the title fits the narrower map canvases
      title = paste0(
        "Areas forecasting an unusually\n", direction, " ",
        trimester, " ", season_year, " season"
      )
    )

  create_images$create_images(
    df_alerts = df_map,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = sea5_anomaly_map,
    image_use = "map",
    width = 6,
    height = 4,
    settings = "map"
  )
}

#' Map SEA5 anomaly data for a single country
#'
#' Recomputes the firing signal for the latest issuance up to the alert date
#' with `alert_sea5_anomaly$signal_shares()`, then shades every admin 1 unit
#' that qualifies for that trimester and direction by its return period class,
#' using the direction's colour scale from `palette_sea5_anomaly`. Units
#' outside their rainy season for that trimester are greyed out and striped;
#' in-season units that do not qualify are shown in the neutral map fill.
#' Boundaries come from the OCHA CODs on fieldmaps.io, the same source as the
#' pcodes in the data. Returns `NULL` when the boundaries cannot be downloaded
#' so the campaign is generated without a map.
#'
#' @param df_wrangled Wrangled data frame for plotting (single alerted country).
#' @param df_raw Raw data frame, not used.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns Admin 1 choropleth ggplot object, or `NULL`
sea5_anomaly_map <- function(df_wrangled, df_raw, title, date) {
  iso3 <- unique(df_wrangled$iso3)
  df_issuance <- dplyr$filter(df_wrangled, date == max(date))
  signal <- df_issuance |>
    alert_sea5_anomaly$signal_shares() |>
    dplyr$slice_head(n = 1)

  sf_adm1 <- tryCatch(
    download_shapefile$download_shapefile(
      url = glue$glue("https://data.fieldmaps.io/cod/originals/{tolower(iso3)}.gpkg.zip"),
      layer = glue$glue("{tolower(iso3)}_adm1")
    ),
    error = \(e) {
      logger$log_warn("No admin 1 boundaries for ", iso3, " on fieldmaps.io: ", e$message)
      NULL
    }
  )
  if (is.null(sf_adm1)) {
    return(NULL)
  }

  df_units <- df_issuance |>
    dplyr$filter(trimester == signal$trimester) |>
    dplyr$mutate(
      rp = if (signal$direction == "dry") dry_rp else wet_rp,
      rp_class = dplyr$case_when(
        !(in_season_flat %in% TRUE) ~ off_season_label,
        alert_sea5_anomaly$unit_qualifies(rp, pearson_r, in_season_flat) ~
          as.character(palette_sea5_anomaly$rp_class(rp)),
        .default = not_alerting_label
      ),
      rp_class = factor(rp_class, levels = class_levels)
    )

  sf_units <- sf_adm1 |>
    dplyr$rename_with(tolower) |>
    dplyr$inner_join(df_units, by = c("adm1_pcode" = "pcode")) |>
    iso3_shift_longitude$iso3_shift_longitude(iso3)

  sf_list <- sf_adm0$sf_adm0(iso3 = iso3, action = "nothing")

  # the data source name is long, so the boundary source goes on its own line
  # to keep the caption inside the narrow map canvases
  map_caption <- caption$caption(indicator_id = "sea5_anomaly", iso3 = iso3, map = TRUE) |>
    sub(pattern = "; Boundaries", replacement = "\nBoundaries", fixed = TRUE)

  class_fills <- stats$setNames(
    c(
      palette_sea5_anomaly$direction_palettes[[signal$direction]],
      palette_sea5_anomaly$below_threshold_fill,
      hdx_signals_palette$map_boundary
    ),
    class_levels
  )

  gg$ggplot() +
    gg$geom_sf(
      data = sf_list$sf_adm0
    ) +
    ggpattern$geom_sf_pattern(
      data = sf_units,
      mapping = gg$aes(fill = rp_class, pattern = rp_class),
      color = "white",
      linewidth = 0.1,
      pattern_fill = hdx_signals_palette$neutral_grey_dark,
      pattern_colour = hdx_signals_palette$neutral_grey_dark,
      pattern_angle = 45,
      pattern_density = 0.3,
      pattern_spacing = 0.02,
      pattern_key_scale_factor = 0.5,
      # draw a swatch for every class, including those absent from this
      # country, so the legend reads the same across campaigns
      show.legend = TRUE
    ) +
    gg$scale_fill_manual(
      values = class_fills,
      drop = FALSE
    ) +
    ggpattern$scale_pattern_manual(
      values = class_patterns,
      drop = FALSE
    ) +
    gg$coord_sf(
      clip = "off",
      crs = "OGC:CRS84"
    ) +
    gg$labs(
      x = "",
      y = "",
      fill = "Return period",
      pattern = "Return period",
      title = title,
      caption = map_caption
    ) +
    map_theme$map_theme(
      iso3 = iso3,
      use_map_settings = TRUE,
      margin_location = "title"
    ) +
    gg$theme(
      legend.key = gg$element_rect(color = hdx_signals_palette$hairline, fill = NA),
      # the class labels are words rather than the short numeric values the
      # shared map theme is sized for, so step the legend text down a little
      legend.title = gg$element_text(size = 14),
      legend.text = gg$element_text(size = 11)
    )
}
