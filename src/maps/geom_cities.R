box::use(gg = ggplot2)
box::use(ggrepel)

box::use(./get_cities_sf)

#' Geom for cities in a country
#'
#' Uses the Natural Earth populated places data to add cities to a map. Some
#' countries have no cities, but often due to size, so we don't add them
#' where they do not exist since they are mainly useful to contextualize locations
#' in larger countries.
#'
#' Adds in the geom for the points and the geom for the labels.
#'
#' @param iso3 ISO3 code
#'
#' @returns geom of cities points and labels
#'
#' @export
geom_cities <- function(iso3) {
  sf_cities <- get_cities_sf$get_cities_sf(iso3)
  list(
    gg$geom_sf(
      data = sf_cities
    ),
    ggrepel$geom_text_repel(
      data = sf_cities,
      mapping = gg$aes(
        label = name,
        geometry = geometry
      ),
      stat = "sf_coordinates",
      min.segment.length = Inf
    )
  )
}
