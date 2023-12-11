box::use(googlesheets4)
box::use(googledrive)
box::use(dplyr)
box::use(stringr)
box::use(arrow)

box::use(../utils/gmas_test_run[gmas_test_run])

#' Read a Google Sheet
#'
#' Finds the specified Google Sheet by looking for the file `name`. Since Google
#' Sheets are stored on Google Drive as "name" without any file extension, the
#' function looks for an exact match to the name in the list of files accessed,
#' adding ^ and $ to denote start and end of string respectively and avoid
#' matching anything like "name.csv". The file is then read in using `read_sheet`.
#'
#' The `sheet` name is
#' specified to be `name`, matching `update_gs_file()`.
#'
#' @param name Name of the Google Sheet.
#' @param col_types Column type specifications as specified in
#'     [googlesheets4::read_sheet].
#'
#' @returns Data frame.
#'
#' @export
read_gs_file <- function(name, col_types, ...) {
  ss <- get_gs_file(paste0("^", name, "$"))
  googlesheets4$read_sheet(
    ss = ss,
    sheet = name,
    col_types = col_types,
    ...
  )
}

#' Read a Parquet file stored on Google Drive
#'
#' Finds the specified Google Drive parquet file by looking for the file `name`.
#' The function looks for an exact match to the name in the list of files accessed,
#' adding ^ and .parquet$ to denote start and end of string respectively and avoid
#' matching anything like "name.csv". The file is then read in using `arrow::read_parquet`.
#'
#' The function uses `get_col_types()` to automatically provide column types
#' when reading sheets, unless `col_types` is not `NULL`. The `sheet` name is
#' specified to be `name`, matching `update_pq_file()`.
#'
#' @param name Name of the file, without the .parquet suffix.
#' @param ... Additional arguments passed to `arrow::read_parquet`.
#'
#' @returns Data frame.
#'
#' @export
read_pq_file <- function(name, ...) {
  file_id <- get_gs_file(paste0("^", name, ".parquet$"))
  tf <- tempfile(fileext = ".parquet")
  googledrive$drive_download(file = file_id, path = tf)
  arrow$read_parquet(
    file = tf,
    ...
  )
}

#' Save data frame to parquet file on Google Drive
#'
#' A convenient file saver that saves the data frame to the specified
#' name. Performs the same file name searching with ^ and .parquet$ as in `read_pq_file()`
#' then simply writes out the data frame using [arrow::write_parquet()] and
#' uploads that to the Google Drive using [googledrive::drive_upload()].
#'
#' @param df Data frame to save out.
#' @param name Name of the file.
#'
#' @export
update_pq_file <- function(df, name) {
  file_id <- get_gs_file(paste0("^", name, ".parquet$"))
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

  googledrive$drive_update(
    file = file_id,
    media = tf
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

##############################
#### SETUP AUTHORIZATIONS ####
##############################

# authorize drive access
googlesheets4$gs4_auth(
  path = Sys.getenv("GLOBAL_MONITORING_JSON")
)

googledrive$drive_auth(
  path = Sys.getenv("GLOBAL_MONITORING_JSON")
)

# list all files that can be accessed
all_files <- googledrive$drive_ls(
  supportsAllDrives = TRUE,
  includeItemsFromAllDrives = TRUE
)

#' Find Google Drive file
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

convert_file <- function(name) {
  ss <- get_gs_file(paste0("^", name, "$"))
  col_types <- get_col_types(name)
  df <- googlesheets4$read_sheet(
    ss = ss,
    sheet = name,
    col_types = col_types
  )
  arrow$write_parquet(
    x = df,
    sink = paste0("/Users/caldwellst/Desktop/", name, ".parquet")
  )
}
