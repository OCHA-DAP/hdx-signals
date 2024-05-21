box::use(readr)

#' Download raw JRC ASAP drought data
#'
#' Downloads raw JRC ASAP drought data from HDX.
#'
#' @export
raw <- function() {
  readr$read_csv(
    file = "https://data.humdata.org/dataset/43b7c86b-8f74-4422-840d-17f30ef3fd2f/resource/b495dbc5-abf4-4efd-9501-3cde16bf23c9/download/asap-hotspots-monthly.csv", # nolint
    col_types = readr$cols()
  )
}
