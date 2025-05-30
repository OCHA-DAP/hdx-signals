box::use(
  dplyr,
  lubridate,
  sf
)

box::use(
  src/utils/location_codes,
  src/utils/formatters,
  src/images/maps/map_points,
  src/images/plots/caption,
  src/images/create_images
)

#' Map conflict events
#'
#' Creates map of conflict events.
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
        "Reported events since ",
        formatters$format_date(date - lubridate$days(30))
      )
    )
  create_images$create_images(
    df_alerts = df_map,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = conflict_map,
    image_use = "map",
    width = 6,
    height = 4,
    settings = "map"
  )
}

#' Map ACLED conflict data
#'
#' Plots conflict data for a specific location, defined by an ISO3 code.
#' Since ACLED events sometimes are recorded as
#' naval battles, we exclude these from mapping as our maps currently exclude
#' national boundaries, water, and other details.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns Plot of cholera for that wrangled data
conflict_map <- function(df_wrangled, df_raw, title, date) {
  caption <- caption$caption(
    indicator_id = "acled_conflict",
    iso3 = unique(df_wrangled$iso3),
    map = TRUE
  )

  iso3 <- unique(df_wrangled$iso3)
  ison <- location_codes$iso3_to_ison(iso3)

  # need to filter raw data and create sf
  # first filter
  sf_raw <- df_raw |>
    dplyr$filter(
      iso == ison,
      event_date >= date - lubridate$days(30),
      event_date <= date,
      !is.na(latitude)
    ) |>
    dplyr$select(
      latitude,
      longitude,
      fatalities
    ) |>
    sf$st_as_sf(
      coords = c("longitude", "latitude"),
      crs = "OGC:CRS84"
    )

  map_points$map_points(
    iso3 = iso3,
    sf_points = sf_raw,
    val_col = "fatalities",
    size = "Fatalities",
    action = "nothing",
    title = title,
    caption = caption,
    settings = "map"
  )
}
