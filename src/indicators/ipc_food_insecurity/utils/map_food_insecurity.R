box::use(
  dplyr,
  stringr,
  gg = ggplot2,
  ripc,
  sf,
  sf
)

box::use(
  src/images/create_images,
  src/images/plots/caption,
  src/images/maps/sf_adm0,
  src/images/maps/geom_cities,
  src/images/maps/map_theme
)

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
    width = 6,
    height = 4,
    use_map_settings = FALSE
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
        ) |>
        dplyr$filter( # drop some extremely complex shapes
          !(stringr$str_detect(sf$st_geometry_type(geometry), "POINT") & is.na(admin_type))
        )
    },
    error = \(e) NULL
  )

  # make sure we catch for the few instances where no map is available via API
  # use NULL because `is.null()` is not vectorized, so easier than `NA`
  if (is.null(sf_ipc)) return(NULL)

  # if points available, separate those out
  geom_type <- sf$st_geometry_type(sf_ipc)
  sf_points <- dplyr$filter(sf_ipc, geom_type == "POINT")
  sf_areas <- dplyr$filter(sf_ipc, geom_type != "POINT")

  sf_list <- sf_adm0$sf_adm0(iso3 = iso3)

  p <- gg$ggplot() +
    gg$geom_sf(
      data = sf_list$sf_adm0
    ) +
    gg$geom_sf(
      data = sf_areas,
      mapping = gg$aes(
        fill = overall_phase
      ),
      color = "white",
      linewidth = 0.1
    ) +
    geom_cities$geom_cities(iso3)

  if (iso3 == "LAC") {
    poly_labels <- lac_labels()

    p <- p +
      gg$geom_sf_text(
        data = poly_labels,
        gg$aes(label = poly_label),
        size = 3,
        fun.geometry = \(x) {
          suppressWarnings( # suppress warnings
            sf$st_point_on_surface(sf$st_zm(x))
          )
        }
      )
  }

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
      fill = "Area phase",
      shape = "Settlement",
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    map_theme$map_theme(
      iso3 = iso3,
      use_map_settings = TRUE,
      margin_location = "subtitle"
    ) +
    gg$guides(
      fill = gg$guide_legend(override.aes = list(shape = NA))
    )
}

#' Produce labels for the LAC product
lac_labels <- function() {
  dplyr$tribble(
    ~X,        ~Y,   ~poly_label,
    -90.6285,  14.83991,   "Guatemala",
    -88.773358, 13.679912, "El Salvador",
    -87.215936, 14.565515,    "Honduras"
  ) |>
    sf$st_as_sf(coords = c("X", "Y"), crs = 4326)
}
