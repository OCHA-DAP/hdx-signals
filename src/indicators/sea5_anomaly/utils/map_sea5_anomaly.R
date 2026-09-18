box::use(
  dplyr,
  gg = ggplot2,
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
  src/utils/download_shapefile,
  src/utils/iso3_shift_longitude
)

# Return period classes for alerting units, light to dark within each direction
rp_breaks <- c(5, 10, 20, Inf)
rp_labels <- c("5 to 10 years", "10 to 20 years", "20 years or more")
not_alerting_label <- "Not alerting"

direction_palettes <- list(
  dry = c("#DDA555", "#B07A2F", "#7F5619"),
  wet = c("#74A1E8", hdx_signals_palette$primary_blue, hdx_signals_palette$primary_blue_dark)
)

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
      title = paste0(
        "Areas forecasting an unusually ", direction, " ",
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
#' that qualifies for that trimester and direction by its return period class.
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
      rp_class = dplyr$if_else(
        alert_sea5_anomaly$unit_qualifies(rp, pearson_r, in_season_flat),
        as.character(cut(rp, breaks = rp_breaks, labels = rp_labels, right = FALSE)),
        not_alerting_label
      ),
      rp_class = factor(rp_class, levels = c(rp_labels, not_alerting_label))
    )

  sf_units <- sf_adm1 |>
    dplyr$rename_with(tolower) |>
    dplyr$inner_join(df_units, by = c("adm1_pcode" = "pcode")) |>
    iso3_shift_longitude$iso3_shift_longitude(iso3)

  sf_list <- sf_adm0$sf_adm0(iso3 = iso3, action = "nothing")

  gg$ggplot() +
    gg$geom_sf(
      data = sf_list$sf_adm0
    ) +
    gg$geom_sf(
      data = sf_units,
      mapping = gg$aes(fill = rp_class),
      color = "white",
      linewidth = 0.1,
      # draw a swatch for every class, including those absent from this
      # country, so the legend reads the same across campaigns
      key_glyph = "rect",
      show.legend = TRUE
    ) +
    gg$scale_fill_manual(
      values = stats$setNames(
        c(direction_palettes[[signal$direction]], hdx_signals_palette$map_fill),
        c(rp_labels, not_alerting_label)
      ),
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
      title = title,
      caption = caption$caption(
        indicator_id = "sea5_anomaly",
        iso3 = iso3,
        map = TRUE,
        extra_boundary_source = "OCHA CODs via fieldmaps.io"
      )
    ) +
    map_theme$map_theme(
      iso3 = iso3,
      use_map_settings = TRUE,
      margin_location = "title"
    ) +
    gg$theme(
      legend.key = gg$element_rect(color = hdx_signals_palette$hairline, fill = NA)
    )
}
