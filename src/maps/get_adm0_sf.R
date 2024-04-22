box::use(glue)
box::use(sf)
box::use(dplyr)
box::use(purrr)
box::use(stringr)
box::use(tools)

box::use(cs = ../utils/cloud_storage)
box::use(./download_shapefile[download_shapefile])

# prevent errors when unioning
suppressMessages(
  sf$sf_use_s2(FALSE)
)

#' Get the ADM0 shapefile for a country
#'
#' Takes in an `iso3` code, and downloads and loads the country data.
#'
#' Once downloaded and loaded, the file is simplified to ensure that only the
#' country boundaries are available as a single row, using `sf::st_union()`.
#' This makes it simple for plotting and for calculating centroids.
#'
#' @param iso3 ISO3 code
#'
#' @returns Shapefile of the country boundaries
#'
#' @export
get_adm0_sf <- function(iso3) {
  sf_adm0 <- download_adm0_sf(iso3)
  suppressMessages(
    sf$st_union(sf_adm0) |>
      sf$st_as_sf() # so we can check number of rows
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
    purrr$map(
      .x = c("GTM", "SLV", "HND"),
      .f = download_adm0_sf
    ) |>
      purrr$reduce(
        .f = sf$st_union
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
