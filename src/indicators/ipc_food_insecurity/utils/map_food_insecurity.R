box::use(dplyr)
box::use(stringr)
box::use(gg = ggplot2)
box::use(ripc)
box::use(sf)
box::use(logger)

box::use(../../../utils/location_codes)
box::use(../../../utils/formatters)
box::use(../../../images/create_images)
box::use(../../../images/plots/caption)

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
        "Area phase classifications, ",
        type,
        "\n",
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
  logger$log_info(iso3)
  caption <- caption$caption(
    indicator_id = "ipc_food_insecurity",
    iso3 = unique(df_wrangled$iso3),
    map = TRUE,
    extra_boundary_source = "IPC/CH",
    extra_caption = "Population in these areas can still be classified in other phases."
  )
  # get title and subtitle from the title
  title_split <- stringr$str_split(string = title, pattern = "\\n", simplify = TRUE)
  title <- title_split[1]
  subtitle <- title_split[2]

  # get information for calling the IPC/CH API
  df_analysis_info <- df_wrangled |>
    dplyr$filter(
      date == max(date, as.Date("1500-01-01"))
    ) |>
    dplyr$summarize(
      id = unique(analysis_id),
      period = if (stringr$str_detect(unique(title)[1], "projected")) "P" else "C"
    )

  # load in areas to map from the IPC/CH API
  sf_ipc <- tryCatch(
    {
      ripc$ipc_get_areas(
        id = df_analysis_info$id,
        period = df_analysis_info$period,
        return_format = "geojson"
      ) |>
        dplyr$mutate(
          overall_phase = as.character(overall_phase),
          point_type = dplyr$case_when(
            admin_type == "idp" ~ "IDP",
            admin_type == "urb" ~ "Urban",
            admin_type == "rfg" ~ "Refugee",
            admin_type == "hhg" ~ "Household group",
            .default = "Rural"
          )
        )
    },
    error = \(e) NULL
  )

  # make sure we catch for the few instances where no map is available via API
  if (is.null(sf_ipc)) return(NA)

  # if points available, separate those out
  geom_type <- sf$st_geometry_type(sf_ipc)
  sf_points <- dplyr$filter(sf_ipc, geom_type == "POINT")
  sf_areas <- dplyr$filter(sf_ipc, geom_type != "POINT")

  p <- gg_map$gg_map(iso3) +
    gg$geom_sf(
      data = sf_areas,
      mapping = gg$aes(
        fill = overall_phase
      ),
      color = "white",
      linewidth = 0.1
    ) +
    geom_cities$geom_cities(iso3)

  # only map points if there are any, avoids warnings for missing scales
  if (nrow(sf_points) > 0) {
    p <- p +
      gg$geom_sf(
        data = sf_points,
        mapping = gg$aes(
          fill = overall_phase,
          shape = point_type
        ),
        color = "black"
      ) +
      gg$scale_shape_manual(
        values = c(
          "Urban" = 21,
          "IDP" = 22,
          "Refugee" = 23,
          "Rural" = 24,
          "Household group" = 25
        )
      )
  }

  p +
    gg$scale_fill_manual(
      values = c(
        "0" = "#FFFFFF",
        "1" = "#CDFACD",
        "2" = "#FAE61E",
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
      shape = "Settlement",
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    map_theme$map_theme(iso3 = iso3, use_map_settings = TRUE)
}
