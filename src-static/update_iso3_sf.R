box::use(glue)
box::use(sf)
box::use(dplyr)
box::use(purrr)
box::use(stringr)

box::use(cs = ../src/utils/cloud_storage)
box::use(./utils/download_shapefile[download_shapefile])
box::use(../src/utils/all_iso3_codes[all_iso3_codes])

# prevent errors when unioning
suppressMessages(
  sf$sf_use_s2(FALSE)
)

###################
#### FUNCTIONS ####
###################

#' Get the ADM0 shapefile for a country
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
#' @returns Shapefile of the country boundaries
#'
#' @export
update_adm0_sf <- function(iso3) {
  sf_adm0 <- download_adm0_sf(iso3)

  sf_union <- suppressMessages(
    sf$st_union(sf_adm0) |>
      sf$st_as_sf() # so we can check number of rows
  )

  if (sf$st_geometry_type(sf_union) == "GEOMETRYCOLLECTION") {
    sf_union <- sf$st_collection_extract(sf_union) |>
      sf$st_union()
  }

  # simply in projected coordinates
  sf_simplified <- sf_union |>
    sf$st_transform(crs = "ESRI:54032") |> # azimuthal equidistant
    sf$st_simplify(
      preserveTopology = TRUE,
      dTolerance = 50
    ) |>
    sf$st_transform(crs = 4326)

  cs$update_az_file(
    df = sf_simplified,
    name = glue$glue("input/adm0/{iso3}.geojson")
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
download_adm0_sf <- function(iso3) {
  if (iso3 == "LAC") {
    # for LAC we get all 3 of El Salvador, Guatemala, and Honduras
    sf$st_union(
      dplyr$filter(sf_world, iso3cd %in% c("SLV", "GTM", "HND"))
    )
  } else if (iso3 == "AB9") {
    download_shapefile("https://open.africa/dataset/56d1d233-0298-4b6a-8397-d0233a1509aa/resource/76c698c9-e282-4d00-9202-42bcd908535b/download/ssd_admbnda_abyei_imwg_nbs_20180401.zip")
  } else if (iso3 == "XKX") {
    download_shapefile("https://data.geocode.earth/wof/dist/shapefile/whosonfirst-data-admin-xk-latest.zip")
  } else if (iso3 == "IOT") {
    download_shapefile("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/world-administrative-boundaries/exports/geojson?lang=en&timezone=Europe%2FLondon") |>
      dplyr$filter(
        iso3 == "IOT"
      )
  } else if (iso3 == "UMI") {
    download_shapefile("https://data.geocode.earth/wof/dist/shapefile/whosonfirst-data-admin-um-latest.zip")
  } else if (iso3 == "WLF") {
    download_shapefile("https://pacificdata.org/data/dataset/0319bab7-4b09-4fcc-81d6-e2c2a8694078/resource/9f6d96d5-02e5-44d8-bb42-4244bde23aa5/download/wf_tsz_pol_april2022.zip")
  } else {
    # try getting cod, and fallback to the UN geodata
    tryCatch(
      suppressWarnings(download_fieldmaps_sf(iso3)),
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
download_fieldmaps_sf <- function(iso3) {
  iso3 <- tolower(iso3)
  download_shapefile(
    url = glue$glue("https://data.fieldmaps.io/cod/originals/{iso3}.gpkg.zip")
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
sf_world <- cs$read_az_file("input/un_geodata.geojson")

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
  fn <- glue$glue("input/adm0/{iso3}.geojson")
  cs$read_az_file(fn) |>
    sf$st_transform(crs = "ESRI:54032") |> # azimuthal equidistant
    sf$st_centroid() |>
    sf$st_transform(crs = 4326) |>
    sf$st_coordinates() |>
    dplyr$as_tibble() |>
    dplyr$transmute(
      iso3 = !!iso3,
      lat = Y,
      lon = X
    ) |>
    cs$update_az_file(
      name = glue$glue("input/centroids/{iso3}.geojson")
    )
}

#' Update bounding boxes for regions
#'
#' Once we have updated country files, we can update again our regional bounding
#' boxes. These are likely not going to be updated on the webmap, but doing just
#' in case they are needed so they are always up to date.
#'
#' For speed, just uses the UN Geodata for calculation.
update_region_bboxes <- function() {
  country_info <- cs$read_az_file("input/country_info.parquet")
  country_info |>
    dplyr$group_by(region) |>
    dplyr$group_map(
      .f = \(df, x) {
        bbox <- sf_world |>
          dplyr$filter(
            iso3cd %in% df$iso3
          ) |>
          sf$st_union() |>
          sf$st_bbox()

        dplyr$tibble(
          xmin = bbox$xmin,
          ymin = bbox$ymin,
          xmax = bbox$xmax,
          ymax = bbox$ymax
        )
      }
    ) |>
    purrr$list_rbind() |>
    cs$update_az_file("input/region_bbox.parquet")
}

################
#### UPDATE ####
################

# first update adm0 files
purrr$walk(
  .x = all_iso3_codes(),
  .f = update_adm0_sf
)

# then update centroids
purrr$walk(
  .x = all_iso3_codes(),
  .f = update_centroids_sf
)

# then update bboxes
update_region_bboxes()
