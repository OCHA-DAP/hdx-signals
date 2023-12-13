box::use(googlesheets4)
box::use(googledrive)
box::use(dplyr)
box::use(stringr)
box::use(readr)
box::use(arrow)
box::use(gcs = googleCloudStorageR)

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
  con <- gzcon(rawConnection(httr::content(object)))
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

#' Save data frame to Google Sheet
#'
#' DEPRECATED: This only exists to save out files for CERF, but will eventually
#' be removed once CERF switches to API access.
#'
#' A convenient file saver that saves the data frame to the specified
#' name. Performs the same file name searching with ^ and $ as in `read_gs_file()`
#' then simply writes out the data frame using [googlesheets4::write_sheet()].
#' The `sheet` name is specified to explicitly be `name`, matching `read_gs_file()`.
#'
#' @param df Data frame to save out.
#' @param name Name of the Google Sheet.
#'
#' @export
update_gs_file <- function(df, name) {
  ss <- get_gs_file(paste0("^", name, "$"))
  if (gmas_test_run()) {
    message(
      "`update_gs_file()` not saving data as `gmas_test_run()` is `TRUE`. ",
      "Set `GMAS_TEST_RUN` env variable to `FALSE` if you want the data to be ",
      "saved, but be careful of sending emails or calling the OpenAI API."
    )
    return(invisible(NULL))
  }
  googlesheets4$write_sheet(
    data = df,
    ss = ss,
    sheet = name
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

# authorize drive access
# TODO: Drop Google Sheets and Drive
googlesheets4$gs4_auth(
  path = Sys.getenv("GLOBAL_MONITORING_JSON")
)

googledrive$drive_auth(
  path = Sys.getenv("GLOBAL_MONITORING_JSON")
)

gcs$gcs_auth(
  json_file = Sys.getenv("GLOBAL_MONITORING_JSON")
)

# list all files that can be accessed
all_files <- googledrive$drive_ls(
  supportsAllDrives = TRUE,
  includeItemsFromAllDrives = TRUE
)

#' Find Google Drive file
#'
#' DEPRECATED: This only exists to save out files for CERF, but will eventually
#' be removed once CERF switches to API access.
#'
#' Searches `all_files` to find the row that matches `name`. These are passed
#' directly to read from and update the Google Sheets via API.
#'
#' @param name Name of the Google Sheet.
#'
#' @returns Google Drive row that matches the name, which can be used to download
#'     to download and update the file.
get_gs_file <- function(name) {
  dplyr$filter(all_files, stringr$str_starts(name, !!name))
}
