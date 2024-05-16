box::use(dplyr)
box::use(stringr)
box::use(gg = ggplot2)
box::use(ripc)

box::use(../../../utils/country_codes)
box::use(../../../utils/formatters)
box::use(../../../images/create_images)

box::use(../../../images/maps/map_points)
box::use(../../../images/maps/gg_map)
box::use(../../../images/maps/geom_cities)
box::use(../../../images/maps/map_theme)

#' Map food insecurity
#'
#' Creates map of food insecurity
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw
#' @param preview Whether or not to preview the plots
#'
#' @export
map <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  df_map <- df_alerts |>
    dplyr$mutate(
      title = paste0(
        "Area classified phase 3 and above, ",
        type,
        ", ",
        map_date
      )
    )

  create_images$create_images(
    df_alerts = df_map,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = food_insecurity_map,
    image_use = "map",
    use_map_settings = TRUE
  )
}

#' Map IPC food insecurity data
#'
#' Plots IPC food insecurity data.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns Plot of cholera for that wrangled data
food_insecurity_map <- function(df_wrangled, df_raw, title, date) {
  iso3 <- unique(df_wrangled$iso3)
  caption <- paste(
    "Data from the IPC/CH, https://www.ipcinfo.org",
    paste("Created", formatters$format_date(Sys.Date())),
    country_codes$iso3_to_names(iso3),
    sep = "\n"
  )

  # get information for calling the IPC/CH API
  df_analysis_info <- df_wrangled |>
    dplyr$filter(
      date == max(date, as.Date("1500-01-01"))
    ) |>
    dplyr$summarize(
      id = unique(analysis_id),
      period = if (stringr$str_detect(title,"projected")) "P" else "C"
    )

  # load in areas to map from the IPC/CH API
  sf_ipc <- ripc$ipc_get_areas(
    id = df_analysis_info$id,
    period = df_analysis_info$period,
    return_format = "geojson"
  ) |>
    dplyr$filter(
      overall_phase >= 3 # only plotting 3 and above
    ) |>
    dplyr$mutate(
      overall_phase = as.character(overall_phase)
    )

  gg_map$gg_map(iso3) +
    gg$geom_sf(
      data = sf_ipc,
      mapping = gg$aes(
        fill = overall_phase
      ),
      color = "white"
    ) +
    geom_cities$geom_cities(iso3) +
    gg$scale_fill_manual(
      values = c(
        "3" = "#E67800",
        "4" = "#C80000",
        "5" = "#640000"
      )
    ) +
    gg$coord_sf(
      clip = "off",
      crs = "OGC:CRS84"
    ) +
    gg$labs(
      x = "",
      y = "",
      fill = "Phase",
      subtitle = title,
      caption = caption
    ) +
    map_theme$map_theme(iso3 = iso3, use_map_settings = TRUE)
}
