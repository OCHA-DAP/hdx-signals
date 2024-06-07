#' In this code we create maps for all 251 iso3 codes
#' For each map a random sample of points is drawn over the bounding box
#' of the admin 0 boundary and plotted.

# Implement map creation
box::use(purrr)

box::use(../../../src/utils/all_iso3_codes)
box::use(./map_test)

  purrr$map(
    .x = all_iso3_codes$all_iso3_codes(),
    .f = \(iso3) {
      map_test$map_test(
        iso3 = iso3,
        sample_n = 1:20,
        sample_values = 1:20000,
        use_bbox = TRUE,
        out_dir = "map_test"
      )
    }
  )
