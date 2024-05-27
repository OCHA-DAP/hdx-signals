#' Script to update country boundaries and then their centroids

box::use(glue)
box::use(sf)
box::use(dplyr)
box::use(purrr)
box::use(stringr)
box::use(lwgeom)
box::use(logger[log_info])

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/download_shapefile[download_shapefile])
box::use(../src/utils/all_iso3_codes[all_iso3_codes])
box::use(../src/utils/get_iso3_sf)

log_info("Updating additional map data...")

# prevent errors when unioning
suppressMessages(
  sf$sf_use_s2(FALSE)
)

###################
#### FUNCTIONS ####
###################

#' Update the ADM0 geojson for a country on Azure blob storage
#'
#' Takes in an `iso3` code, and downloads and loads the country data.
#'
#' Once downloaded and loaded, the file is simplified to ensure that only the
#' country boundaries are available as a single row, using `sf::st_union()`.
#' This makes it simple for plotting and for calculating centroids. The file
#' is then upload to Azure in `input/adm0/{iso3}.geojson`.
#'
#' @param iso3 ISO3 code
#'
#' @returns Nothing
#'
#' @export
update_adm0_sf <- function(iso3) {
  sf_simplified <- simplify_adm0(iso3)
  cs$update_az_file(
    df = sf_simplified,
    name = glue$glue("input/adm0/{iso3}.geojson")
  )
}

#' Download, load, and simplify geojson admin 0 county boundary from iso3 code
#'
#' Takes in an `iso3` code, and downloads and loads the country data.
#'
#' Once downloaded and loaded, the file is simplified to ensure that only the
#' country boundaries are available as a single row, using `sf::st_union()`.
#' This makes it simple for plotting and for calculating centroids.
#'
#' @param iso3 ISO3 code
#'
#' @returns sf class object contain admin 0 country boundary
#'
#' @export
simplify_adm0 <- function(iso3){
  sf_adm0 <- download_adm0_sf(iso3) |>
    filter_adm0_sf(iso3)

  sf_union <- suppressMessages(
    sf_adm0 |>
      dplyr$group_by(data_source) |>
      dplyr$summarise(do_union = TRUE)
  )

  # have to extract geometries from collections
  if (sf$st_geometry_type(sf_union) == "GEOMETRYCOLLECTION") {
    sf_union <- sf$st_collection_extract(sf_union) |>
      dplyr$group_by(data_source) |>
      dplyr$summarise(do_union = TRUE)
  }

  # simply in projected coordinates
  sf_simplified <- sf_union |>
    sf$st_transform(crs = "ESRI:54032") |> # azimuthal equidistant
    sf$st_simplify(
      preserveTopology = TRUE,
      dTolerance = 50
    ) |>
    sf$st_transform(crs = "OGC:CRS84")

  sf_simplified
}

#' Filter ADM0 shapefile
#'
#' Filters the shapefile for a given `iso3` code. Used when countries have
#' small territories far from the main country area in the shapefile, which
#' makes plotting extremely difficult. Filtering only setup for these
#' specific `iso3` codes.
#'
#' @param sf_adm0 ADM0 shapefile for the country
#' @param iso3 ISO3 code
#'
#' @returns Filtered `sf_adm0`
filter_adm0_sf <- function(sf_adm0, iso3) {
  if (iso3 == "CHL"){
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, xmin = 33.73339)
  } else if (iso3 == "BMU") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, ymax = -0.05)
  } else if (iso3 == "CRI") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, ymin = 0.1)
  } else if (iso3 == "ECU") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, xmin = 3)
  } else if (iso3 == "ESP") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, ymin = 1.8)
  } else if (iso3 == "FJI") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, ymax = -0.05, ymin = 0.5)
  } else if (iso3 == "GNQ") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, ymin = 0.5)
  } else if (iso3 == "JAM") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, ymin = 0.5)
  } else if (iso3 == "NZL") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, xmin = 1)
  } else if (iso3 == "PRT") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, xmin = 20)
  } else if (iso3 == "USA") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, xmin = 30, ymax = -19)
  } else if (iso3 == "VNM") {
    sf_adm0 <- st_crop_adj_bbox(sf_adm0, xmax = -8, ymin = 1)
  }

  sf_adm0
}

#' Crop with adjusted bbox
#'
#' Gets the bbox for an `sf` object, and then adjusts it based on the parameters
#' based in, then crops the `sf`.
#'
#' @param sf_obj Simple feature object
#' @param xmin Amount to adjust xmin
#' @param xmax Amount to adjust xmax
#' @param ymin Amount to adjust ymin
#' @param ymax Amount to adjust ymax
#'
#' @returns Cropped simple feature
st_crop_adj_bbox <- function(sf_obj, xmin = 0, xmax = 0, ymin = 0, ymax = 0) {
  old_bbox <- sf$st_bbox(sf_obj)
  sf$st_crop(
    sf_obj,
    xmin = old_bbox[["xmin"]] + xmin,
    xmax = old_bbox[["xmax"]] + xmax,
    ymin = old_bbox[["ymin"]] + ymin,
    ymax = old_bbox[["ymax"]] + ymax
  )
}

#' Download ADM0 shapefile
#'
#' Takes in an `iso3` code, and returns the country shapefile for that country.
#'
#' For some `iso3` codes, custom files are required, and these are handled directly
#' within this function. However, for the rest, the default is to try and pull
#' the COD shapefile from Fieldmaps (because it has standardized columns and files
#' for easy access). If CODs are unavailable, then the country shape is pulled
#' from the UN Geodata downloaded from the geodata portal.
#'
#' https://geoportal.un.org/arcgis/home/item.html?id=d7caaff3ef4b4f7c82689b7c4694ad92
#'
#' @param iso3 ISO3
#' @param update_azure `logical` if TRUE (default) update json file in azure
#'     if FALSE
download_adm0_sf <- function(iso3,update_azure = TRUE) {
  if (iso3 == "LAC") {
    # for LAC we get all 3 of El Salvador, Guatemala, and Honduras
    sf$st_union(
      dplyr$filter(sf_world, iso3cd %in% c("SLV", "GTM", "HND"))
    )
  } else if (iso3 == "AB9") {
    download_shapefile(
      url = "https://open.africa/dataset/56d1d233-0298-4b6a-8397-d0233a1509aa/resource/76c698c9-e282-4d00-9202-42bcd908535b/download/ssd_admbnda_abyei_imwg_nbs_20180401.zip", # nolint
      layer = "ssd_admbnda_abyei_imwg_nbs_20180401",
      data_source  = "Open Africa"
    )
  } else if (iso3 == "XKX") {
    download_shapefile(
      url = "https://data.geocode.earth/wof/dist/shapefile/whosonfirst-data-admin-xk-latest.zip",
      layer = "whosonfirst-data-admin-xk-country-polygon",
      data_soure ="Who's On First"
    )
  } else if (iso3 == "IOT") {
    download_shapefile(
      url = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/world-administrative-boundaries/exports/geojson?lang=en&timezone=Europe%2FLondon",
      data_source = "Opendatasoft"
      ) |> # nolint
      dplyr$filter(
        iso3 == "IOT"
      )

  } else if (iso3 == "UMI") {
    download_shapefile(
      url = "https://data.geocode.earth/wof/dist/shapefile/whosonfirst-data-admin-um-latest.zip",
      data_soure ="Who's On First"
      )
  } else if (iso3 == "WLF") {
    download_shapefile(
      url = "https://pacificdata.org/data/dataset/0319bab7-4b09-4fcc-81d6-e2c2a8694078/resource/9f6d96d5-02e5-44d8-bb42-4244bde23aa5/download/wf_tsz_pol_april2022.zip",
      data_source = "Pacific Data Hub"
      ) # nolint
  } else if (iso3 == "FJI") {
    # make sure that we shift the coordinates so it plots correctly
    download_fieldmaps_sf("FJI") |>
      sf$st_shift_longitude()
  } else {
    # first try getting cod, and fallback to the UN geodata
    tryCatch(
      suppressWarnings(download_fieldmaps_sf(iso3,layer = glue$glue("{tolower(iso3)}_adm0"))),
      error = \(e) get_un_geodata(iso3)
    )
  }
}

#' Download and load adm0 boundaries
#'
#' Downloads the adm0 shapefile from fieldmaps, because it sources OCHA boundaries
#' but standardizes column names. Then it loads the file in using `sf::st_read()`.
#'
#' @param iso3 ISO3 code
download_fieldmaps_sf <- function(iso3, layer=NULL) {
  iso3 <- tolower(iso3)
  download_shapefile(
    url = glue$glue("https://data.fieldmaps.io/cod/originals/{iso3}.gpkg.zip"),
    data_source = "FieldMaps",
    layer = layer
  )
}

#' Get UN geodata for that country
#'
#' UN Geodata stored on Azure is used if no COD shapefile is available and no
#' other alternate data source is specified.
get_un_geodata <- function(iso3) {
  if (iso3 %in% sf_world$iso3cd) {
    dplyr$filter(sf_world, iso3cd == iso3)
  } else {
    stop(
      stringr$str_wrap(
        paste0(
          iso3,
          " data not available in the 'un_geodata.geojson' file. Add alternative ",
          "method for accessing data to `get_adm0_sf()`."
        )
      )
    )
  }
}

#' Loading in module level
sf_world <- cs$read_az_file("input/un_geodata.geojson") |>
  dplyr$mutate(
    data_source = "UN Geo Hub"
  )

#' Update the centroids for ISO3
#'
#' Done using this method to ensure that if errors are generated, progress is not
#' lost. So incrementally load the adm0 boundaries for a country, calculate the centroid,
#' then store on Azure.
#'
#' If a centroid is incorrect or needs adjusting due to strange geometries for the
#' ISO3 code, specific adjustments can be made to the function below to catch
#' specific errors
#'
#' @param iso3 ISO3 code
#'
#' @returns
update_centroids_sf <- function(iso3) {
  get_iso3_sf$get_iso3_sf(iso3) |>
    sf$st_centroid() |>
    sf$st_coordinates() |>
    dplyr$as_tibble() |>
    dplyr$transmute(
      iso3 = !!iso3,
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

log_info("...Updating ADM0 files...")

# first update adm0 files
purrr$walk(
  .x = all_iso3_codes(),
  .f = update_adm0_sf
)

log_info("...Updating centroids...")

# then update centroids
purrr$walk(
  .x = all_iso3_codes(),
  .f = update_centroids_sf
)

log_info("Successfully updated map data")
