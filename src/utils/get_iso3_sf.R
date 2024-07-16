box::use(glue)
box::use(rlang)
box::use(sf)

box::use(cs = ./cloud_storage)
box::use(./iso3_shift_longitude)

#' Get the ADM0 shapefile for a location
#'
#' Takes in an `iso3` code, and reads the location data from Azure.
#'
#' These files have already been processed. Boundaries and centroids are processed
#' in `src-static/update_iso3_sf`. Cities are processed in `src-static/update_cities_sf.R`.
#' If you need to update the data for a specific
#' location, ensure the update is done in those scripts to update everything on Azure.
#'
#' `NULL` is returned if the file is missing. The files for `adm0` and `centroids`
#' are complete for `all_iso3_codes()`. However, some will be missing for `cities`
#' because these are not necessary for smaller location maps.
#'
#' @param iso3 ISO3 code
#' @param file File to return, either adm0, centroids, or cities.
#'
#' @returns Shapefile, unless missing, in which case `NULL` is returned.
#'
#' @export
get_iso3_sf <- function(iso3, file = c("adm0", "centroids", "cities")) {
  file <- rlang$arg_match(file)
  fileext <- if (file == "centroids") "parquet" else "geojson"
  fn <- glue$glue("input/{file}/{iso3}.{fileext}")
  if (!(fn %in% azure_files)) {
    return(NULL)
  }

  df <- cs$read_az_file(
    fn
  )

  if (file == "centroids") {
    sf$st_as_sf(
      x = df,
      coords = c("lon", "lat"),
      crs = "OGC:CRS84"
    ) |>
      iso3_shift_longitude$iso3_shift_longitude(iso3)
  } else {
    iso3_shift_longitude$iso3_shift_longitude(df, iso3)
  }
}


azure_files <- cs$az_file_detect()
