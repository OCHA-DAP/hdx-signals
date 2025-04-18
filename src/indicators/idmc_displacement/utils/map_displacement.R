box::use(
  dplyr,
  lubridate,
  sf
)

box::use(
  src/utils/formatters,
  src/images/maps/map_points,
  src/images/plots/caption,
  src/images/create_images
)

#' Map displacement events
#'
#' Creates map of displacement events
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
        "Displacement events since ",
        formatters$format_date(date - lubridate$days(30))
      )
    )

  create_images$create_images(
    df_alerts = df_map,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = displacement_map,
    image_use = "map",
    width = 6,
    height = 4,
    settings = "map"
  )
}

#' Map IDMC displacement data
#'
#' Plots displacement data for a specific location, defined by an ISO3 code.
#' Since the IDMC data is often erroneously placed in other countries or the
#' middle of open water, we generate errors if the points don't overlap the
#' admin boundaries.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns Plot of cholera for that wrangled data
displacement_map <- function(df_wrangled, df_raw, title, date) {
  caption <- caption$caption(
    indicator_id = "idmc_displacement_conflict", # same values as disaster
    iso3 = unique(df_wrangled$iso3),
    map = TRUE
  )

  iso3 <- unique(df_wrangled$iso3)

  # need to filter raw data and create sf
  # first filter
  sf_raw <- df_raw |>
    dplyr$filter(
      iso3 %in% !!iso3,
      displacement_end_date >= (date - lubridate$days(30)),
      displacement_start_date <= date | Sys.Date() - displacement_start_date <= 90,
      !is.na(latitude)
    ) |>
    dplyr$select(
      latitude,
      longitude,
      figure
    ) |>
    sf$st_as_sf(
      coords = c("longitude", "latitude"),
      crs = "OGC:CRS84"
    )

  map_points$map_points(
    iso3 = iso3,
    sf_points = sf_raw,
    val_col = "figure",
    size = "Displacements",
    action = "error",
    title = title,
    caption = caption
  )
}
