box::use(
  gg = ggplot2
)

box::use(
  src/images/create_images,
  src/images/plots/caption,
  src/images/maps/sf_adm0,
  src/images/maps/map_theme
)

#' Map SEA5 anomaly
#'
#' Creates an adm0-level map for SEA5 anomaly alerts.
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw Raw data
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
    width = 6,
    height = 4,
    settings = "map"
  )
}

#' Map SEA5 anomaly data at adm0 level
#'
#' Produces an adm0-level map showing the country boundary for the alert.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns adm0 map for the alert location
sea5_anomaly_map <- function(df_wrangled, df_raw, title, date) {
  iso3 <- unique(df_wrangled$iso3)

  map_caption <- caption$caption(
    indicator_id = "sea5_anomaly",
    iso3 = iso3,
    map = TRUE
  )

  sf_list <- sf_adm0$sf_adm0(iso3 = iso3)

  gg$ggplot() +
    gg$geom_sf(
      data = sf_list$sf_adm0
    ) +
    gg$coord_sf(
      clip = "off",
      crs = "OGC:CRS84"
    ) +
    gg$labs(
      x = "",
      y = "",
      title = title,
      caption = map_caption
    ) +
    map_theme$map_theme(
      iso3 = iso3,
      use_map_settings = TRUE,
      margin_location = "subtitle"
    )
}
