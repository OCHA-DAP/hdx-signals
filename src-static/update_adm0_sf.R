#' Script to update location boundaries

box::use(glue)
box::use(sf)
box::use(dplyr)
box::use(purrr)
box::use(stringr)
box::use(logger)

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/download_shapefile)
box::use(../src/utils/all_iso3_codes)
box::use(../src/utils/hs_logger)
box::use(../src/utils/st_crop_adj_bbox)

hs_logger$configure_logger()

logger$log_info("Updating ADM0 data...")

# prevent errors when unioning
suppressMessages(
  sf$sf_use_s2(FALSE)
)

###################
#### FUNCTIONS ####
###################

#' Update the ADM0 geojson for a location on Azure blob storage
#'
#' Takes in an `iso3` code, and downloads and loads the location data.
#'
#' Once downloaded and loaded, the file is simplified to ensure that only the
#' location boundaries are available as a single row, using `sf::st_union()`.
#' This makes it simple for plotting and for calculating centroids. The file
#' is then upload to Azure in `input/adm0/{iso3}.geojson`.
#'
#' @param iso3 ISO3 code
#'
#' @returns Nothing
#'
#' @export
update_adm0_sf <- function(iso3) {
  sf_simplified <- download_adm0_sf(iso3) |>
    sf$st_set_agr("constant") |>
    filter_adm0_sf(iso3) |>
    simplify_adm0()

  cs$update_az_file(
    df = sf_simplified,
    name = glue$glue("input/adm0/{iso3}.geojson")
  )
}

#' Download, load, and simplify geojson admin 0 county boundary from iso3 code
#'
#' Takes in an adm0 spatial file and simplifies it.
#'
#' Once downloaded and loaded, the file is simplified to ensure that only the
#' location boundaries are available as a single row, using `sf::st_union()`.
#' This makes it simple for plotting and for calculating centroids.
#'
#' @param sf ISO3 code
#'
#' @returns sf class object contain admin 0 location boundary
#'
#' @export
simplify_adm0 <- function(sf_adm0) {
  sf_union <- suppressMessages(
    sf_adm0 |>
      dplyr$group_by(boundary_source) |>
      dplyr$summarise(do_union = TRUE)
  )

  # have to extract geometries from collections
  if (sf$st_geometry_type(sf_union) == "GEOMETRYCOLLECTION") {
    sf_union <- sf$st_collection_extract(sf_union) |>
      dplyr$group_by(boundary_source) |>
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
#' Filters the shapefile for a given `iso3` code. Used when locations have
#' small territories far from the main location area in the shapefile, which
#' makes plotting extremely difficult. Filtering only setup for these
#' specific `iso3` codes.
#'
#' @param sf_adm0 ADM0 shapefile for the location
#' @param iso3 ISO3 code
#'
#' @returns Filtered `sf_adm0`
filter_adm0_sf <- function(sf_adm0, iso3) {
  if (iso3 == "CHL") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, xmin = 33.73339)
  } else if (iso3 == "BMU") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, ymax = -0.05)
  } else if (iso3 == "CRI") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, ymin = 0.1)
  } else if (iso3 == "ECU") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, xmin = 3)
  } else if (iso3 == "ESP") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, ymin = 1.8)
  } else if (iso3 == "FJI") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, ymax = -0.05, ymin = 0.5)
  } else if (iso3 == "GNQ") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, ymin = 0.5)
  } else if (iso3 == "JAM") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, ymin = 0.5)
  } else if (iso3 == "NZL") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, xmin = 1)
  } else if (iso3 == "PRT") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, xmin = 20)
  } else if (iso3 == "USA") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, xmax = -100, ymin = 3.4) # drop hawaii and guam
  } else if (iso3 == "VNM") {
    sf_adm0 <- st_crop_adj_bbox$st_crop_adj_bbox(sf_adm0, xmax = -8, ymin = 1)
  }

  sf_adm0
}

#' Download ADM0 shapefile
#'
#' Takes in an `iso3` code, and returns the location shapefile for that location
#'
#' For some `iso3` codes, custom files are required, and these are handled directly
#' within this function. However, for the rest, the default is to try and pull
#' the COD shapefile from FieldMaps (because it has standardized columns and files
#' for easy access). If CODs are unavailable, then the location shape is pulled
#' from the UN Geodata downloaded from the geodata portal.
#'
#' https://geoportal.un.org/arcgis/home/item.html?id=d7caaff3ef4b4f7c82689b7c4694ad92
#'
#' @param iso3 ISO3
download_adm0_sf <- function(iso3) {
  if (iso3 == "LAC") {
    # for LAC we get all 3 of El Salvador, Guatemala, and Honduras
    dplyr$filter(sf_world, iso3 %in% c("SLV", "GTM", "HND")) |>
      dplyr$group_by(iso3) |>
      # pull together polygons by iso3
      dplyr$summarise(do_union = TRUE, .groups = "drop") |>
      # then merge all iso3s into 1 multipolygon retaining boundaries
      dplyr$summarise(iso3 = "LAC", boundary_source = "UN Geo Hub", do_union = FALSE)

  } else if (iso3 == "AB9") {
    download_shapefile$download_shapefile(
      url = "https://open.africa/dataset/56d1d233-0298-4b6a-8397-d0233a1509aa/resource/76c698c9-e282-4d00-9202-42bcd908535b/download/ssd_admbnda_abyei_imwg_nbs_20180401.zip", # nolint
      layer = "ssd_admbnda_abyei_imwg_nbs_20180401",
      iso3 = iso3,
      boundary_source  = "Open Africa"
    )
  } else if (iso3 == "XKX") {
    download_shapefile$download_shapefile(
      url = "https://data.geocode.earth/wof/dist/shapefile/whosonfirst-data-admin-xk-latest.zip",
      layer = "whosonfirst-data-admin-xk-country-polygon",
      iso3 = iso3,
      boundary_source = "Who's On First"
    )
  } else if (iso3 == "BLM") {
    download_shapefile$download_shapefile(
      url = "https://data.humdata.org/dataset/41f80e67-f140-494b-9b34-7861c4951364/resource/93991ce2-fcc0-476a-9cd4-44e12c002f55/download/blm_adm0.zip", #nolint
      layer = "BLM_adm0",
      boundary_source = "HDX, OCHA"
    )
  } else if (iso3 == "IOT") {
    download_shapefile$download_shapefile(
      url = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/world-administrative-boundaries/exports/geojson?lang=en&timezone=Europe%2FLondon", #nolint
      boundary_source = "Opendatasoft"
    ) |>
      dplyr$filter(
        iso3 == "IOT"
      )

  } else if (iso3 == "UMI") {
    download_shapefile$download_shapefile(
      url = "https://data.geocode.earth/wof/dist/shapefile/whosonfirst-data-admin-um-latest.zip",
      boundary_source  = "Who's On First",
      iso3 = iso3,
      layer = "whosonfirst-data-admin-um-region-polygon"
    )
  } else if (iso3 == "WLF") {
    download_shapefile$download_shapefile(
      url = "https://pacificdata.org/data/dataset/0319bab7-4b09-4fcc-81d6-e2c2a8694078/resource/9f6d96d5-02e5-44d8-bb42-4244bde23aa5/download/wf_tsz_pol_april2022.zip",#nolint
      boundary_source = "Pacific Data Hub",
      iso3 = iso3,
      layer = "wf_tsz_pol_april2022"
    ) # nolint
  } else if (iso3 == "FJI") {
    # make sure that we shift the coordinates so it plots correctly
    download_fieldmaps_sf("FJI", "fji_adm0") |>
      sf$st_shift_longitude()
  } else {
    # first try getting cod, and fallback to the UN geodata
    tryCatch(
      suppressWarnings(download_fieldmaps_sf(iso3, layer = glue$glue("{tolower(iso3)}_adm0"))),
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
download_fieldmaps_sf <- function(iso3, layer = NULL) {
  iso3 <- tolower(iso3)
  download_shapefile$download_shapefile(
    url = glue$glue("https://data.fieldmaps.io/cod/originals/{iso3}.gpkg.zip"),
    layer = layer,
    iso3 = iso3,
    boundary_source = "FieldMaps, OCHA"
  )
}

#' Get UN geodata for that location
#'
#' UN Geodata stored on Azure is used if no COD shapefile is available and no
#' other alternate data source is specified.
get_un_geodata <- function(iso3) {
  if (iso3 %in% sf_world$iso3) {
    dplyr$filter(sf_world, iso3 == !!iso3)
  } else {
    stop(
      stringr$str_wrap(
        paste0(
          iso3,
          " data not available in the 'un_geodata_complex.geojson' file. Add alternative ",
          "method for accessing data to `get_adm0_sf()`."
        )
      ),
      call. = FALSE
    )
  }
}

#' Read in the data and just keep iso3 and boundary_source columns
sf_world <- cs$read_az_file("input/un_geodata_complex.geojson") |>
  dplyr$transmute(
    iso3 = ISO3CD,
    boundary_source = "UN Geo Hub"
  )

################
#### UPDATE ####
################

purrr$walk(
  .x = all_iso3_codes$all_iso3_codes(),
  .f = update_adm0_sf,
  .progress = interactive()
)

logger$log_info("Successfully updated ADM0 data")
