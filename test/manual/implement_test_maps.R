#' In this code we create maps for all 251 iso3 codes
#' For each map a random sample of points is drawn over the bounding box
#' of the admin 0 boundary and plotted.

# Implement map creation
box::use(../../src/images/maps/map_test)
box::use(purrr[map])
box::use(../../src/utils/all_iso3_codes[all_iso3_codes])

all_iso3 <- all_iso3_codes()

all_iso3 |>
  map(
    \(iso3_tmp) {
      ggsave_map_points_test(
        iso3 = iso3_tmp,
        use_bbox = TRUE,
        pt_value_name = "value",
        pt_value_label = "Test Bubble Legend",
        sample_pt_number_range = 1:20,
        sample_pt_value_range = 1:20000,
        subtitle = paste0(iso3_tmp, " Test Map"),
        map_settings = df_map_settings,
        out_dir = "map_test"
      )
    }
  )
