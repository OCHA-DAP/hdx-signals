box::use(glue)
box::use(sf)
box::use(dplyr)
box::use(rnaturalearth)
box::use(purrr)
box::use(stringr)
box::use(tools)

#' Get the adm0 shapefile for that iso3 code
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
    download_fieldmaps_sf(iso3)
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

#' Download and wrangle cities data
#'
#' Downloads cities data from Natural Earth. Since some countries have too many
#' cities (8 for the USA) while others have none (need to make cities for Latin
#' America and the Caribbean LAC iso3 used by the IPC).
#'
get_cities_sf <- function(iso3) {
  pop_sf <- rnaturalearth$ne_download(
    type = "populated_places",
    category = "cultural"
  )
}
