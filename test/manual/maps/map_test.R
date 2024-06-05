box::use(gg = ggplot2)
box::use(scales)
box::use(gghdx)
box::use(sf)
box::use(dplyr)

box::use(../../../src/images/plots/theme_signals)
box::use(../../../src/images/maps/gg_map)
box::use(../../../src/images/maps/geom_cities)
box::use(../../../src/images/maps/geom_centroids)
box::use(../../../src/images/maps/map_points)
box::use(../../../src/utils/get_iso3_sf)
box::use(cs = ../../../src/utils/cloud_storage)

df_map_settings <- cs$read_az_file("input/iso3_map_settings.json")

#' Create and save plots with randomly sampled values over admin zone of interest
#' files will be saved in the directory specified with syntax: "{iso3}_map.png"
#'
#' This function is implemented for all iso3s in tests/manual/implement_test_maps.R
#'
#' @param iso3 ISO3 code
#' @param sample_n `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param sample_values `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#' @param map_settings json file containing map specifications (default = "input/iso3_map_settings.json")
#' @param out_dir `character` directory to output sample maps. If NULL (default) no image written.
#'    and no image written
#'
#' @return ggplot image
#' @export
map_test <- function(
  iso3,
  sample_n = 1:20,
  sample_values = 1:20000,
  use_bbox = TRUE,
  map_settings = df_map_settings,
  out_dir = NULL
) {

  # filter map settings for dimensions
  df_ms <- dplyr$filter(df_map_settings, iso3 == !!iso3)

  # make plot
  p <- map_with_points_test(
    iso3,
    sample_n = sample_n,
    sample_values = sample_values,
    use_bbox = use_bbox
  )

  if (!is.null(out_dir)) {
    # make file path
    base_fp <- paste0(iso3, "_map", ".png")
    fp <- file.path(out_dir, base_fp)

    gg$ggsave(
      filename = fp,
      plot = p,
      width = df_ms$width,
      height = df_ms$height,
      units = "in",
      bg = "white"
    )
  }
  return(p)
}

#' Plot randomly generated points over basemap created from in map_points.R
#' User provides iso3 code and sampling specs and function returns location-level
#' plot with spatially random points containing random values used to size the
#' point geoms.
#'
#' @param iso3 ISO3 code
#' @param sample_n `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param sample_values `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#'
#' @return plot ggplot
map_with_points_test  <- function(
  iso3,
  sample_n = 1:20,
  sample_values = 1:20000,
  use_bbox = TRUE
) {

  gdf_sample_pts <- sample_pts_iso3(
    iso3 = iso3,
    sample_n = sample_n,
    sample_values = sample_values,
    use_bbox = use_bbox
  )

  map_points$map_points(
    iso3 = iso3,
    df = gdf_sample_pts,
    val_col = "value",
    size = "Test bubble",
    subtitle = iso3
  )
}
#' Generate pts for test map from iso3 code
#'
#' @param iso3 ISO3 code
#' @param sample_n `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param sample_values `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#'
#' @return sf class data.frame with point geometry
sample_pts_iso3 <- function(
    iso3,
    sample_n = 1:20,
    sample_values = 1:20000,
    use_bbox = TRUE) {
  gdf_adm0 <- get_iso3_sf$get_iso3_sf(iso3, "adm0")

  random_spatial_sample(
    poly = gdf_adm0,
    use_bbox = use_bbox,
    sample_n = sample_n,
    sample_values = sample_values
  )
}

#' Randomly generate points within polygon
#'
#' Generates a point sample within a sample with a flexible number of points and point values.
#' This is used withing the map_test() function to simulate points for visual inspection.
#'
#' @param poly sf class polygon
#' @param sample_n `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param sample_values `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#'
#' @return sf class data.frame with point geometry
#'
#' @examples
#' library(sf)
#' library(dplyr)
#' file_name <- system.file("shape/nc.shp", package="sf")
#' nc_counties <- st_read(file_name)
#' alleghany_county <- nc_counties |>
#'     filter(NAME == "Alleghany")
#'
#' pts_sampled_bbox <- random_spatial_sample(
#'                      poly = alleghany_county,
#'                      use_bbox = TRUE,
#'                      sample_n = 1:20,
#'                      sample_values = 1:20000
#'                      )
random_spatial_sample <- function(
    poly,
    use_bbox = TRUE,
    sample_n = 1:20,
    sample_values = 1:20000) {

  num_pts <- sample(x = sample_n, size = 1, replace = TRUE)

  if (use_bbox) {
    sample_region <- sf$st_bbox(poly) |>
      sf$st_as_sfc()
  } else {
    sample_region <- poly
  }

  pts_sample <- sf$st_sample(x = sample_region, size = num_pts) |>
    sf$st_as_sf()

  pts_sample$value <- sample(sample_values, num_pts, replace = TRUE)
  pts_sample
}
