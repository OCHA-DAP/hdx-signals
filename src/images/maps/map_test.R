box::use(gg = ggplot2)
box::use(scales)
box::use(gghdx)

box::use(../plots/theme_signals)
box::use(./gg_map)
box::use(./geom_cities)
box::use(./geom_centroids)
box::use(./iso3_ggsave)

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
