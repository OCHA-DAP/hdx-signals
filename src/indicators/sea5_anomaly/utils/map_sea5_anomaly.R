box::use(
  dplyr,
  gg = ggplot2,
  rnaturalearth,
  scales
)

box::use(
  src/images/create_images,
  src/images/plots/caption,
  src/images/maps/map_theme
)

#' Map SEA5 anomaly
#'
#' Creates a global choropleth map for SEA5 anomaly, showing p_5rp values
#' for the next valid trimester from the forecast date across all countries.
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
    width = 10,
    height = 5,
    settings = "plot"
  )
}

#' Map SEA5 anomaly data at global level
#'
#' Produces a global choropleth map showing all countries colored by their
#' p_5rp values (probability of exceeding the 5-year return period) for
#' the next valid trimester from the forecast date.
#'
#' @param df_wrangled Wrangled data frame for plotting (single alerted country).
#' @param df_raw Raw data frame (all countries).
#' @param title Plot title.
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

  # filter df_raw to the most recent forecast date up to the alert date
  alert_date <- date
  df_map_data <- df_raw |>
    dplyr$filter(date <= alert_date) |>
    dplyr$filter(date == max(date))

  # get world country boundaries
  sf_world <- rnaturalearth$ne_countries(scale = "medium", returnclass = "sf")

  # join world boundaries with p_5rp values by ISO3 code
  sf_data <- sf_world |>
    dplyr$left_join(
      dplyr$select(df_map_data, iso3, p_5rp),
      by = c("iso_a3" = "iso3")
    )

  map_caption <- caption$caption(
    indicator_id = "sea5_anomaly",
    iso3 = unique(df_wrangled$iso3),
    map = FALSE
  )

  gg$ggplot() +
    gg$geom_sf(
      data = sf_data,
      mapping = gg$aes(fill = p_5rp),
      color = "white",
      linewidth = 0.1
    ) +
    gg$scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      limits = c(0, 1),
      na.value = "grey90",
      labels = scales$label_percent(),
      name = "Probability"
    ) +
    gg$coord_sf(
      clip = "off",
      crs = "OGC:CRS84"
    ) +
    gg$labs(
      x = "",
      y = "",
      title = paste0(
        "Probability of exceeding 5-year return period (",
        trimester_label,
        ")"
      ),
      caption = map_caption
    ) +
    map_theme$map_theme(
      iso3 = unique(df_wrangled$iso3),
      use_map_settings = FALSE,
      margin_location = "title"
    )
}
