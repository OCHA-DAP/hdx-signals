box::use(
  gg = ggplot2,
  grid,
  sf
)

box::use(src/images/maps[...])

## Fixtures ------------------------------------------------------------------
##
## A tall, narrow shape (like Afghanistan) that leaves spare width on a 6x4
## canvas once the title and caption are accounted for.

sf_tall <- sf$st_sf(
  geometry = sf$st_sfc(
    sf$st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 3), c(0, 3), c(0, 0)))),
    crs = "OGC:CRS84"
  )
)

tall_map <- function(crs = "OGC:CRS84") {
  gg$ggplot() +
    gg$geom_sf(data = sf_tall) +
    gg$coord_sf(clip = "off", crs = crs) +
    gg$labs(title = "A title", caption = "A caption")
}

test_that("fit_canvas() widens a narrow map's limits to the canvas", {
  p <- tall_map()
  fitted <- fit_canvas(p, width = 6, height = 4)

  x_range_before <- gg$ggplot_build(p)$layout$panel_params[[1]]$x_range
  x_range_after <- gg$ggplot_build(fitted)$layout$panel_params[[1]]$x_range

  expect_gt(diff(x_range_after), diff(x_range_before))
})

test_that("fit_canvas() keeps the map centered", {
  fitted <- fit_canvas(tall_map(), width = 6, height = 4)
  x_range <- gg$ggplot_build(fitted)$layout$panel_params[[1]]$x_range

  expect_equal(mean(x_range), 0.5)
})

test_that("fit_canvas() grows the height, not the width, of a map that is already wide", {
  wide <- gg$ggplot() +
    gg$geom_sf(
      data = sf$st_sf(
        geometry = sf$st_sfc(
          sf$st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 1), c(0, 1), c(0, 0)))),
          crs = "OGC:CRS84"
        )
      )
    ) +
    gg$coord_sf(clip = "off", crs = "OGC:CRS84")

  fitted <- fit_canvas(wide, width = 6, height = 4)
  y_range_before <- gg$ggplot_build(wide)$layout$panel_params[[1]]$y_range
  y_range_after <- gg$ggplot_build(fitted)$layout$panel_params[[1]]$y_range

  expect_gt(diff(y_range_after), diff(y_range_before))
  expect_equal(
    gg$ggplot_build(fitted)$layout$panel_params[[1]]$x_range,
    gg$ggplot_build(wide)$layout$panel_params[[1]]$x_range
  )
})

test_that("fit_canvas() returns plots without a longitude/latitude coord_sf() as is", {
  p_cartesian <- gg$ggplot(data.frame(x = 1:2, y = 1:2), gg$aes(x, y)) + gg$geom_point()
  expect_identical(fit_canvas(p_cartesian, width = 6, height = 4), p_cartesian)

  p_projected <- tall_map(crs = "EPSG:3857")
  expect_identical(fit_canvas(p_projected, width = 6, height = 4), p_projected)
})
