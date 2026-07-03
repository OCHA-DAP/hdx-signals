box::use(cs = src/utils/cloud_storage)

#' Download raw SEA5 anomaly data
#'
#' Reads raw SEA5 anomaly data from the Azure blob storage.
#'
#' @export
raw <- function() {
  cs$read_az_file("output/sea5_anomaly/raw.parquet")
}
