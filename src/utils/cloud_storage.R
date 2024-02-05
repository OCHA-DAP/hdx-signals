box::use(stringr)
box::use(readr)
box::use(arrow)
box::use(httr)

box::use(../utils/gmas_test_run[gmas_test_run])

#' Read a Parquet file stored on Google Cloud Platform bucket
#'
#' Reads a file from the `hdx-signals` bucket.
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
  arrow$read_parquet(
    bucket$path(name)
  )
}

#' Write data frame on Google Cloud Platform
#'
#' A convenient file saver that saves the data frame to the specified
#' parquet file. Simply writes out the data frame based on the file extension and
#' uploads that to the GCS Bucket using [arrow::gs_bucket()] file systems.
#' Currently only supports Apache Parquet files.
#'
#' Files written out based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()]
#'
#' If `gmas_test_run()`, the file is not uploaded to the bucket.
#'
#' @param df Data frame to save out.
#' @param name Name of the file to read, including prefix (`input/` or `output/`)
#'     and filetype `.parquet`.
#'
#' @returns Nothing, but file is written to the `hdx-signals` bucket.
#'
#' @export
update_gcs_file <- function(df, name) {
  if (gmas_test_run()) {
    message(
      "`update_pq_file()` not saving data as `gmas_test_run()` is `TRUE`. ",
      "Set `GMAS_TEST_RUN` env variable to `FALSE` if you want the data to be ",
      "saved, but be careful of sending emails or calling the OpenAI API."
    )
    return(invisible(NULL))
  }

  arrow$write_parquet(
    x = df,
    sink = bucket$path(name)
  )
}

#' Find GCS file names matching pattern
#'
#' Pulls names from the `hdx-signals` bucket and
#' filters those names by the passed patterns, if passed. If `pattern` is `NULL`,
#' then all files in the bucket are returned.
#'
#' @param pattern Pattern to look for. Passed to [stringr::str_detect()]
#'
#' @export
gcs_file_detect <- function(pattern = NULL) {
  file_names <- bucket$ls(recursive = TRUE)
  if (!is.null(pattern)) {
    file_names <- file_names[stringr$str_detect(file_names, pattern)]
  }
  file_names
}

######################
#### SETUP BUCKET ####
######################

# this bucket used to read and write from the GCS parquet
bucket <- arrow$gs_bucket(
  bucket = "hdx-signals",
  json_credentials = Sys.getenv("HDX_SIGNALS_JSON")
)
