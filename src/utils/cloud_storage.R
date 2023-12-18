box::use(stringr)
box::use(readr)
box::use(arrow)
box::use(gcs = googleCloudStorageR)
box::use(httr)

box::use(../utils/gmas_test_run[gmas_test_run])

#' Read a Parquet file stored on Google Drive
#'
#' Reads a file from the `global-monitoring` bucket.
#' The file is read based on its prefix. Currently, the only support is for
#' Apache Parquet files, but other support can be added if necessary.
#'
#' Function parsing is done based on file type:
#'
#' * Apache Parquet: [arrow::read_parquet()].
#'
#' @param name Name of the file to read, including prefix (`input/` or `output/`)
#'     and filetype `.parquet`.
#'
#' @returns Data frame.
#'
#' @export
read_gcs_file <- function(name) {
  gcs$gcs_get_object(
    object_name = name,
    bucket = "global-monitoring",
    parseFunction = gcs_parse_parquet
  )
}

#' Parse parquet file
#'
#' Helper function to parse parquet file.
#'
#' @param object Object to parse
gcs_parse_parquet <- function(object) {
  con <- gzcon(rawConnection(httr$content(object)))
  on.exit(close(con))
  file_raw <- readr$read_file_raw(con)
  file_buffer <- arrow$buffer(file_raw)
  arrow$read_parquet(file_buffer)
}

#' Save data frame on Google Cloud Platform
#'
#' A convenient file saver that saves the data frame to the specified
#' parquet file. Simply writes out the data frame based on the file extension and
#' uploads that to the GCS Bucket using [googleCloudStorageR::gcs_upload()].
#' Currently only supports Apache Parquet files.
#'
#' Files written out based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()]
#'
#' If `gmas_test_run()`, the file is written out to a temporary file but is not
#' uploaded to the bucket.
#'
#' @param df Data frame to save out.
#' @param name Name of the file to read, including prefix (`input/` or `output/`)
#'     and filetype `.parquet`.
#'
#' @returns Nothing, but file is saved to the `global-monitoring` bucket.
#'
#' @export
update_gcs_file <- function(df, name) {
  tf <- tempfile(fileext = ".parquet")
  arrow$write_parquet(
    x = df,
    sink = tf
  )

  if (gmas_test_run()) {
    message(
      "`update_pq_file()` not saving data as `gmas_test_run()` is `TRUE`. ",
      "Set `GMAS_TEST_RUN` env variable to `FALSE` if you want the data to be ",
      "saved, but be careful of sending emails or calling the OpenAI API."
    )
    return(invisible(NULL))
  }

  invisible(
    gcs$gcs_upload(
      file = tf,
      bucket = "global-monitoring",
      name = name,
      predefinedAcl = "bucketLevel"
    )
  )
}

#' Find GCS file names matching pattern
#'
#' Pulls names from the `global-monitoring` bucket and
#' filters those names by the passed patterns, if passed. If `pattern` is `NULL`,
#' then all files in the bucket are returned.
#'
#' @param pattern Pattern to look for. Passed to [stringr::str_detect()]
#'
#' @export
gcs_file_detect <- function(pattern = NULL) {
  file_names <- gcs$gcs_list_objects(bucket = "global-monitoring")$name
  if (!is.null(pattern)) {
    file_names <- file_names[stringr$str_detect(file_names, pattern)]
  }
  file_names
}

##############################
#### SETUP AUTHORIZATIONS ####
##############################

gcs$gcs_auth(
  json_file = Sys.getenv("GLOBAL_MONITORING_JSON")
)
