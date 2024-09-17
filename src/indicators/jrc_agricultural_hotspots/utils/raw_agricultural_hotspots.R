box::use(
  readr,
  utils,
  dplyr
)

box::use(src/utils/location_codes)

#' Download raw JRC ASAP drought data
#'
#' Downloads raw JRC ASAP drought data from HDX.
#'
#' @export
raw <- function() {
  utils$download.file(
    url = "https://agricultural-production-hotspots.ec.europa.eu/files/hotspots_html_ts.zip",
    destfile = zf <- tempfile(fileext = ".zip"),
    quiet = TRUE
  )

  utils$unzip(
    zipfile = zf,
    exdir = td <- tempdir()
  )

  readr$read_delim(
    file = file.path(
      td, "hotspots_html_ts.csv"
    ),
    col_types = readr$cols(),
    delim = ";"
  ) |>
    dplyr$mutate(
      iso3 = location_codes$asap_to_iso3(asap0_id)
    )
}
