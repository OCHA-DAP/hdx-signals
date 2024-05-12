box::use(gg = ggplot2)
box::use(scales)
box::use(gghdx)
box::use(../plots/theme_signals)
box::use(./gg_map)
box::use(./geom_cities)
box::use(./geom_centroids)

box::use(sf) # used to generate points

# commenting out as it creates errors, but im not sure yet what
# to replace it with
# box::use(./iso3_ggsave)


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
map_test <- function(
    iso3, dir
) {
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
  return(p)
  #
  #   iso3_ggsave$iso3_ggsave(
  #     p = p,
  #     iso3 = iso3,
  #     fp = file.path(
  #       dir,
  #       paste0(iso3, ".png")
  #     ),
  #     crop = FALSE
  #   )
  #
  #   invisible(NULL)
}


#' Generate pts for test map
#'
#' @param poly
#' @param number_pt_range `integer` vector (default = 1:20) sampled to randomly decide how many
#'  pts will be mapped
#' @param value_range `integer` vector (defaults 1:20000) sampled to assign values to pts.
#'  might be useful to have it sample a log distribution instead
#' @param use_bbox `logical` if TRUE (default) samples will be drawn from the bounding box
#'  of the polygon, if FALSE sampled withing the polygon. It's mainly a feature to
#'  ease development as sampling withing bbox is much faster
#' @return
#' @export
#'
#' @examples \dontrun{
#' box::use(sf)
#' box::use(../../utils/get_iso3_sf)
#' gdf_adm0 <- get_iso3_sf$get_iso3_sf("AFG")
#' pts_sampled_bbox <- gen_pts(poly = gdf_adm0,
#'                             use_bbox=T,
#'                              number_pt_range= 1:20,
#'                              value_range= 1:20000)
#' }

random_spatial_sample <- function(poly,
                                  use_bbox =T,
                                  number_pt_range= 1:20,
                                  value_range= 1:20000
){
  num_pts <- sample(x= number_pt_range,size = 1,replace=T)

  if(use_bbox){
    gdf_region <- sf$st_bbox(poly) |>
      sf$st_as_sfc()
  } else{
    gdf_region <- poly
  }

  pts_gdf <- sf$st_sample(x=gdf_region,size =num_pts) |>
    sf$st_as_sf()

  pts_gdf$value <- sample(value_range, num_pts, replace = T)
  return(pts_gdf)
}
