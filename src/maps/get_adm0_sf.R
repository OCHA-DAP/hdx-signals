box::use(glue)
box::use(sf)
box::use(dplyr)
box::use(purrr)
box::use(stringr)
box::use(tools)

box::use(cs = ../utils/cloud_storage)

#' Get the ADM0 shapefile for a country
#'
#' Takes in an `iso3` code, and returns the country shapefile for that country.
#' For some `iso3` codes, custom files are required, and these are handled directly
#' within this function. However, for the rest, the default is to try and pull
#' the COD shapefile from Fieldmaps (because it has standardized columns and files
#' for easy access). If CODs are unavailable, then the country shape is pulled
#' from the UN Geodata downloaded from the geodata portal.
#'
#' https://geoportal.un.org/arcgis/home/item.html?id=d7caaff3ef4b4f7c82689b7c4694ad92
#'
#' @param iso3 ISO3 code
#'
#' @returns Shapefile of the country boundaries
#'
#' @export
get_adm0_sf <- function(iso3) {
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
    fn <- download_file("https://open.africa/dataset/56d1d233-0298-4b6a-8397-d0233a1509aa/resource/76c698c9-e282-4d00-9202-42bcd908535b/download/ssd_admbnda_abyei_imwg_nbs_20180401.zip")
    sf$read_sf(fn, quiet = TRUE)
  } else if (iso3 == "XKX") {
    fn <- download_file("https://data.geocode.earth/wof/dist/shapefile/whosonfirst-data-admin-xk-latest.zip")
    sf$read_sf(fn, quiet = TRUE)
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
  fn <- download_file(
    url = glue$glue("https://data.fieldmaps.io/cod/originals/{iso3}.gpkg.zip")
  )

  sf$st_read(
    dsn = fn,
    layer = glue$glue("{iso3}_adm0"),
    quiet = TRUE
  )
}

#' Download file to temp file
#'
#' Download file to temp file, unzipping zip files if necessary. Deals wth zipped
#' files like geojson or gpkg files as well as shapefiles, when the unzipping
#' returns a folder
#'
#' @param url URL to download
download_file <- function(url) {
  if (stringr$str_ends(url, ".zip")) {
    download.file(
      url = url,
      destfile = zf <- tempfile(fileext = ".zip"),
      quiet = TRUE
    )

    unzip(
      zipfile = zf,
      exdir = td <- tempdir()
    )

    # if the file extension is just `.zip`, we return the temp dir alone
    # because that works for shapefiles, otherwise we return the file unzipped
    fn <- stringr$str_remove(basename(url), ".zip")
    if (tools$file_ext(fn) == "") {
      return(td)
    } else {
      return(
        file.path(
          td,
          fn
        )
      )
    }
  } else {
    download.file(
      url = url,
      destfile = tf <- tempfile(fileext = paste0(".", tools$file_ext(url))),
      quiet = TRUE
    )
    return(tf)
  }
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
