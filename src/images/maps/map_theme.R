box::use(
  gg = ggplot2,
  dplyr
)

box::use(
  cs = src/utils/cloud_storage,
  src/images/plots/theme_signals
)


#' Produces theme changes specific to maps
#'
#' Produces theme changes specific to maps. `iso3` code is used to pull in the
#' specific settings if `use_map_settings` is `TRUE`.
#'
#' If `use_map_settings` is `FALSE`, defaults to provid
#'
#' @param iso3 ISO3 code
#' @param use_map_settings Whether or not to use map settings in `input/iso3_map_settings.json`
#'     to position the legend.
#'
#' @returns ggplot theme object
#'
#' @export
map_theme <- function(iso3, use_map_settings = TRUE, margin_location = c("title", "subtitle")) {
  if (use_map_settings) {
    df_ms <- dplyr$filter(df_map_settings, iso3 == !!iso3)
  } else {
    # default values
    df_ms <- data.frame(
      legend_position = "left",
      justification = "top",
      location = "plot",
      direction = "vertical"
    )
  }

  theme_signals$theme_signals(margin_location = margin_location, x_axis_ticks = FALSE) +
    gg$theme(
      rect = gg$element_blank(),
      axis.ticks = gg$element_blank(),
      axis.text.x = gg$element_blank(),
      axis.text.y = gg$element_blank(),
      panel.grid.major = gg$element_blank(),
      panel.grid.minor = gg$element_blank(),
      axis.line.x = gg$element_blank(),
      legend.position = df_ms$legend_position,
      legend.direction = df_ms$direction,
      legend.justification = df_ms$justification,
      legend.location = df_ms$location
    )
}

df_map_settings <- cs$read_az_file("input/iso3_map_settings.json")
