box::use(gg = ggplot2)
box::use(scales)
box::use(gghdx)

box::use(../plots/theme_signals)
box::use(./gg_map)
box::use(./geom_cities)
box::use(./geom_centroids)
box::use(./iso3_ggsave) # prob can delete
box::use(./map_points)
box::use(../../utils/get_iso3_sf)
box::use(sf)
box::use(cs = ../../utils/cloud_storage)
box::use(dplyr)
df_map_settings <- cs$read_az_file("input/iso3_map_settings.json")


#' **Q** Should delete?
#' Test map for ISO3
#'
#' Generates a test map for ISO3 code. Adds country boundaries, centroid, and
#' cities, then saves the plot out to
#'
#' @param iso3 ISO3 code used to get base plot and cities
#' @param dir Directory to save the test plots to
#'
#' @returns Nothing
#'
#' @export
map_test <- function(iso3, dir) {
  p <- gg_map$gg_map(iso3) +
    geom_centroids$geom_centroids(iso3) +
    geom_cities$geom_cities(iso3) +
    gg$coord_sf(
      clip = "off",
      crs = "OGC:CRS84"
    ) +
    gg$labs(
      x = "",
      y = "",
      title = paste0("Test map for ", iso3),
      subtitle = "",
      caption = "Test map generated with country boundaries, centroid, and cities."
    ) +
    gg$theme(
      rect = gg$element_blank(),
      axis.ticks = gg$element_blank(),
      axis.text.x = gg$element_blank(),
      axis.text.y = gg$element_blank(),
      panel.grid.major = gg$element_blank(),
      panel.grid.minor = gg$element_blank(),
      axis.line.x = gg$element_blank(),
      plot.caption = gg$element_text(hjust = 1),
      legend.position = "left",
      legend.direction = "vertical"
    )

  iso3_ggsave$iso3_ggsave(
    p = p,
    iso3 = iso3,
    fp = file.path(
      dir,
      paste0(iso3, ".png")
    ),
    crop = FALSE
  )

  invisible(NULL)
}


#' Create and save plots with randomly sampled values over admin zone of interest
#' files will be saved in the directory specified with syntax: "{iso3}_map.png"
#'
#' This function is implemented for all iso3s in tests/manual/implement_test_maps.R
#'
#' @param iso3 ISO3 code
#' @param pt_value_name `character` name of column that holds the point value
#' @param pt_value_label `character` label to used as legend title over proportional dots
#' @param sample_pt_number_range `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param sample_pt_value_range `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#' @param subtitle `character` subtitle to label plot
#' @param map_settings json file containing map specifications (default = "input/iso3_map_settings.json")
#' @param out_dir `character` directory to output sample maps
#'
#' @return Nothing
#' @export

ggsave_map_points_test <- function(
    iso3,
    use_bbox = TRUE,
    pt_value_name,
    pt_value_label,
    sample_pt_number_range = 1:20,
    sample_pt_value_range = 1:20000,
    subtitle,
    map_settings = df_map_settings,
    out_dir) {

  # make file path
  base_fp <- paste0(iso3, "_map", ".png")
  fp <- file.path(out_dir, base_fp)

  # filter map settings for dimensions
  df_ms <- dplyr$filter(df_map_settings, iso3 == !!iso3)

  # make plot
  p <- map_with_points_test(
    iso3,
    use_bbox = use_bbox,
    pt_value_name = pt_value_name,
    pt_value_label = pt_value_label,
    sample_pt_number_range = sample_pt_number_range,
    sample_pt_value_range = sample_pt_value_range,
    subtitle = subtitle
  )

  gg$ggsave(
    filename = fp,
    plot = p,
    width = df_ms$width,
    height = df_ms$height,
    units = "in"
  )
}


#' Plot randomly generated points over basemap created from in map_points.R
#'
#' @param iso3 ISO3 code
#' @param pt_value_name `character` name of column that holds the point value
#' @param pt_value_label `character` label to used as legend title over proportional dots
#' @param sample_pt_number_range `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param sample_pt_value_range `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#' @param subtitle `character` subtitle to label plot
#'
#' @return plot ggplot
#' @export

map_with_points_test  <- function(
    iso3,
    pt_value_name,
    pt_value_label,
    sample_pt_number_range = 1:20,
    sample_pt_value_range = 1:20000,
    use_bbox = TRUE,
    subtitle) {

  gdf_sample_pts <- sample_pts_iso3(iso3 = iso3,
                                    use_bbox = use_bbox,
                                    number_pt_range = sample_pt_number_range,
                                    value_range = sample_pt_value_range)

  map_points$map_points(iso3 = iso3,
                        df = gdf_sample_pts,
                        pt_value_name = pt_value_name,
                        pt_value_label = pt_value_label,
                        subtitle = subtitle)
}

#' Generate pts for test map from iso3 code
#'
#' @param iso3 ISO3 code
#' @param number_pt_range `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param value_range `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#'
#' @return sf class data.frame with point geometry
#' @export

sample_pts_iso3 <- function(
    iso3,
    use_bbox = TRUE,
    number_pt_range = 1:20,
    value_range = 1:20000) {
  gdf_adm0 <- get_iso3_sf$get_iso3_sf(iso3, "adm0")

  random_spatial_sample(
    poly = gdf_adm0,
    use_bbox = use_bbox,
    number_pt_range = number_pt_range,
    value_range = value_range
  )
}

#' Generate pts for test map
#'
#' @param poly sf class polygon
#' @param number_pt_range `integer` vector (default = 1:20) sampled to randomly decide how many
#'    pts will be mapped
#' @param value_range `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'    might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'    of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'    ease development as sampling withing bbox is much faster
#'
#' @return sf class data.frame with point geometry
#' @export
#'
#' @examples \dontrun{
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
#'                      number_pt_range = 1:20,
#'                      value_range = 1:20000
#'                      )
#' }
random_spatial_sample <- function(
    poly,
    use_bbox = TRUE,
    number_pt_range = 1:20,
    value_range = 1:20000) {
  num_pts <- sample(x = number_pt_range, size = 1, replace = TRUE)

  if (use_bbox) {
    sample_region <- sf$st_bbox(poly) |>
      sf$st_as_sfc()
  } else {
    sample_region <- poly
  }

  pts_sample <- sf$st_sample(x = sample_region, size = num_pts) |>
    sf$st_as_sf()

  pts_sample$value <- sample(value_range, num_pts, replace = TRUE)
  pts_sample
}
