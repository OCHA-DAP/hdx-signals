box::use(
  stringr,
  utils,
  tools,
  sf
)

#' Download shapefile and read
#'
#' Download shapefile to temp file, unzipping zip files if necessary. Deals with zipped
#' files like geojson or gpkg files as well as shapefiles, when the unzipping
#' returns a folder. The file is then read with `sf::st_read()`.
#'
#' @param url URL to download
#' @param layer Layer to read
#' @param iso3 `character` string of ISO3 code to add to the file.
#' @param boundary_source `character` name of source for the admin 0 boundaries
#'     layer. If supplied a column named "boundary_source"
#'     will added to sf object with the specified input. If `NULL` (default)
#'     no column added.
#'
#' @returns sf object
#'
#' @export
download_shapefile <- function(
  url,
  layer = NULL,
  iso3 = NULL,
  boundary_source = NULL
) {
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
    ret <- sf$st_read(
      fn,
      layer = layer,
      quiet = TRUE
    )
  } else {
    ret <- sf$st_read(
      fn,
      quiet = TRUE
    )
  }

  # add in iso3 and boundary source. if NULL, no change will happen
  ret$iso3 <- iso3
  ret$boundary_source <- boundary_source

  ret
}
