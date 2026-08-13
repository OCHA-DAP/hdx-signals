#' @export
box::use(
  src/images/maps/geom_centroids[...],
  src/images/maps/geom_cities[...],
  src/images/maps/map_points[...],
  src/images/maps/map_theme[...],
  src/images/maps/sf_adm0[...]
)

if (is.null(box::name())) {
  box::use(src/images/maps/`__tests__`)
}
