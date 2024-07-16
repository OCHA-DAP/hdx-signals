#' Script to update location boundaries and then their centroids

box::use(glue)
box::use(sf)
box::use(dplyr)
box::use(purrr)
box::use(logger)

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/all_iso3_codes)
box::use(../src/utils/get_iso3_sf)
box::use(../src/utils/hs_logger)

hs_logger$configure_logger()

logger$log_info("Updating centroids data...")

# prevent errors when unioning
suppressMessages(
  sf$sf_use_s2(FALSE)
)

###################
#### FUNCTIONS ####
###################

#' Update the centroids for ISO3
#'
#' Done using this method to ensure that if errors are generated, progress is not
#' lost. So incrementally load the adm0 boundaries for a location, calculate the centroid,
#' then store on Azure. It also adds in the `boundary_source` column to the centroids
#' file as well, which is best for transparency and also allows us to bring it
#' into the location metadata in `update_location_metadata.R`.
#'
#' If a centroid is incorrect or needs adjusting due to strange geometries for the
#' ISO3 code, specific adjustments can be made to the function below to catch
#' specific errors.
#'
#' @param iso3 ISO3 code
#'
#' @returns
update_centroids_sf <- function(iso3) {
  iso3_sf <- get_iso3_sf$get_iso3_sf(iso3)
  iso3_sf <- sf$st_set_agr(iso3_sf, "constant") # remove messaging on attrs

  iso3_sf |>
    sf$st_transform(crs = "ESRI:54032") |> # azimuthal equidistant
    sf$st_centroid() |>
    sf$st_transform(crs = "OGC:CRS84") |>
    sf$st_coordinates() |>
    dplyr$as_tibble() |>
    dplyr$transmute(
      iso3 = !!iso3,
      boundary_source = unique(iso3_sf$boundary_source),
      lat = Y,
      lon = ifelse(X <= 180, X, X - 360)
    ) |>
    cs$update_az_file(
      name = glue$glue("input/centroids/{iso3}.parquet")
    )
}

################
#### UPDATE ####
################

# then update centroids
purrr$walk(
  .x = all_iso3_codes$all_iso3_codes(),
  .f = update_centroids_sf,
  .progress = interactive()
)

logger$log_info("Successfully updated centroids")
