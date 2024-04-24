box::use(gg = ggplot2)

box::use(./geom_adm0)
box::use(./geom_cities)

#' Create base map for country
#'
#' Creates the base map for a country. Initializes `ggplot2::ggplot()`, adds the
#' country boundaries base layer and ensures coordinates are clipped.
#'
#' @param iso3 ISO3 code
#'
#' @returns ggplot object
#'
#' @export
gg_map <- function(iso3) {
  gg$ggplot() +
    geom_adm0$geom_adm0(iso3) +
    gg$coord_sf(
      clip = "off"
    )
}
