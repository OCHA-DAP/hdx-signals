box::use(gg = ggplot2)

box::use(../../utils/get_iso3_sf)

#' Geom for country centroid
#'
#' Adds country centroid for ISO3 code. Since `get_adm0_sf()` looks for custom
#' basemaps, then OCHA CODs, then UN Geodata services by filtering, we throw
#' an error if the returned data is `NULL` or a 0 row data frame.
#'
#' @param iso3 ISO3 code
#'
#' @returns Geom for the country centroid
#'
#' @export
geom_centroids <- function(iso3) {
  sf_adm0 <- get_iso3_sf$get_iso3_sf(iso3, "centroids")

  if (is.null(sf_adm0) || nrow(sf_adm0) == 0) {
    stop(
      "No country boundaries data for ",
      iso3,
      ". Ensure that the country is handled in `get_adm0_sf()`.",
      call. = FALSE
    )
  }

  gg$geom_sf(data = sf_adm0)
}