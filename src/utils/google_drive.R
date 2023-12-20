# TODO: Deprecate entire module once CERF moves away from the current system

box::use(googlesheets4)
box::use(googledrive)
box::use(dplyr)
box::use(stringr)

box::use(../utils/gmas_test_run[gmas_test_run])

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
