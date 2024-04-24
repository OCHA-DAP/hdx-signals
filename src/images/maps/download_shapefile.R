box::use(stringr)
box::use(utils)
box::use(tools)
box::use(sf)

#' Download shapefile and read
#'
#' Download shapefile to temp file, unzipping zip files if necessary. Deals with zipped
#' files like geojson or gpkg files as well as shapefiles, when the unzipping
#' returns a folder. The file is then read with `sf::st_read()`.
#'
#' @param url URL to download
#' @param layer Layer to read
#'
#' @returns sf object
#'
#' @export
download_shapefile <- function(url, layer = NULL) {
  if (stringr$str_ends(url, ".zip")) {
    utils$download.file(
      url = url,
      destfile = zf <- tempfile(fileext = ".zip"),
      quiet = TRUE
    )

    utils$unzip(
      zipfile = zf,
      exdir = td <- tempdir()
    )

    # if the file extension is just `.zip`, we return the temp dir alone
    # because that works for shapefiles, otherwise we return the file unzipped
    fn <- stringr$str_remove(basename(url), ".zip")
    if (tools$file_ext(fn) == "") {
      fn <- td
    } else {
      fn <- file.path(td, fn)
    }
  } else {
    utils$download.file(
      url = url,
      destfile = fn <- tempfile(fileext = paste0(".", tools$file_ext(url))),
      quiet = TRUE
    )
  }

  if (!is.null(layer)) {
    sf$st_read(
      fn,
      layer = layer,
      quiet = TRUE
    )
  } else {
    sf$st_read(
      fn,
      quiet = TRUE
    )
  }
}

