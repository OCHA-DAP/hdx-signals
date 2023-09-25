box::use(googlesheets4)
box::use(googledrive)
box::use(dplyr)
box::use(../utils/gmas_test_run[gmas_test_run])

#' Read a Google Sheet
#'
#' Finds the specified Google Sheet by looking for the file `name`. Since Google
#' Sheets are stored on Google Drive as "name" without any file extension, the
#' function looks for an exact match to the name in the list of files accessed,
#' adding ^ and $ to denote start and end of string respectively and avoid
#' matching anything like "name.csv". The file is then read in using `read_sheet`.
#'
#' The function uses `get_col_types()` to automatically provide column types
#' when reading sheets, unless `col_types` is not `NULL`. The `sheet` name is
#' specified to be `name`, matching `update_gs_file()`.
#'
#' @param name Name of the Google Sheet.
#' @param col_types Column type specifications as specified in
#'     [googlesheets4::read_sheet]. If `NULL`, applies the column types from
#'     `get_col_types()`
#'
#' @returns Data frame.
#'
#' @export
read_gs_file <- function(name, col_types = NULL, ...) {
  ss <- get_gs_file(paste0("^", name, "$"))
  if (is.null(col_types)) {
    col_types <- get_col_types(name)
  }
  googlesheets4$read_sheet(
    ss = ss,
    sheet = name,
    col_types = col_types,
    ...
  )
}

#' Save data frame to Google Sheet
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

#' Retrieve static column types for specified sheet
#'
#' Since column types are static for most datasets read through the global
#' monitoring platform, this stores those types by name to automatically use
#' with `read_gs_file()`. This was done because certain unexpected errors were
#' occurring when date columns were being read in as POSIX types.
#'
#' @param name Name of the Google Sheet.
get_col_types <- function(name) {
  # flags files all the same
  if (name %in% c("flags_idmc", "flags_cholera", "flags_total", "flags_test")) {
    col_types <- "ccccDDccclc"
  } else if (name == "flags_ipc") {
    col_types <- "ccccDDDcclc"
  } else if (name == "flags_total_daily") {
    col_types <- "ccccDDDccclc"
  } else if (name == "flags_emailed") {
    col_types <- "ccccDDccccD"
  } else if (name == "raw_idmc") {
    col_types <- "dccddcccdDDDdcDDllllcccclD"
  } else if (name == "raw_ipc") {
    col_types <- "cccDccccDDdddddddddddddcc"
  } else if (name == "raw_cholera") {
    col_types <- "c"
  } else if (name == "wrangled_idmc") {
    col_types <- "ccDddddddlllllldl"
  } else if (name == "wrangled_ipc") {
    col_types <- "cccccccDDDldddddddddddddddddddd"
  } else if (name == "wrangled_cholera") {
    col_types <- "cDcDd"
  } else if (name %in% c("cerf_dashboard_names", "idmc_country_links")) {
    col_types <- "cc"
  } else if (name == "email_recipients") {
    col_types <- "ccll"
  } else {
    NULL
  }
}
