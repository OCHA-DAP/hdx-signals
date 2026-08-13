## Fixtures ------------------------------------------------------------------
##
## `sf_display` stands in for a trimmed display basemap (like the real USA or
## Chile files, which drop Hawaii/Guam or Easter Island). `sf_full` stands in
## for the untrimmed boundary `get_iso3_sf_full()` falls back to, which
## additionally covers a remote territory the display map doesn't show.

sf_display <- sf$st_sf(
  geometry = sf$st_sfc(
    sf$st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = "OGC:CRS84"
  )
)

sf_remote_territory <- sf$st_polygon(
  list(rbind(c(50, 50), c(51, 50), c(51, 51), c(50, 51), c(50, 50)))
)

sf_full <- sf$st_sf(
  geometry = sf$st_sfc(
    sf$st_union(sf_display$geometry[[1]], sf_remote_territory),
    crs = "OGC:CRS84"
  )
)

make_point <- function(x, y) {
  sf$st_sf(figure = 1, geometry = sf$st_sfc(sf$st_point(c(x, y)), crs = "OGC:CRS84"))
}

# inside the display boundary
inside_pt <- make_point(0.5, 0.5)
# inside the remote territory only the full boundary covers
remote_pt <- make_point(50.5, 50.5)
# nowhere near either boundary
bad_pt <- make_point(200, 200)

## Tests -----------------------------------------------------------------

test_that("sf_adm0() passes through points covered by the display boundary", {
  stub(sf_adm0, "get_iso3_sf$get_iso3_sf", sf_display)

  result <- sf_adm0("ZZZ", action = "error", inside_pt)

  expect_equal(nrow(result$additional_geoms[[1]]), 1)
})

test_that("sf_adm0() drops points only covered by the full, untrimmed boundary", {
  stub(sf_adm0, "get_iso3_sf$get_iso3_sf", sf_display)
  stub(sf_adm0, "get_iso3_sf_full", sf_full)

  result <- sf_adm0("ZZZ", action = "error", rbind(inside_pt, remote_pt))

  expect_equal(nrow(result$additional_geoms[[1]]), 1)
  expect_equal(as.numeric(sf$st_coordinates(result$additional_geoms[[1]])), c(0.5, 0.5))
})

test_that("sf_adm0() still errors when a point is covered by neither boundary", {
  stub(sf_adm0, "get_iso3_sf$get_iso3_sf", sf_display)
  stub(sf_adm0, "get_iso3_sf_full", sf_full)

  expect_error(
    sf_adm0("ZZZ", action = "error", bad_pt),
    "not contained within 0.2 degrees"
  )
})

test_that("sf_adm0() errors when no full boundary exists to fall back on", {
  stub(sf_adm0, "get_iso3_sf$get_iso3_sf", sf_display)
  stub(sf_adm0, "get_iso3_sf_full", NULL)

  expect_error(
    sf_adm0("ZZZ", action = "error", remote_pt),
    "not contained within 0.2 degrees"
  )
})
