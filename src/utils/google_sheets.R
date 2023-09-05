library(googlesheets4)
library(googledrive)
library(readr)

# authorize drive access
gs4_auth(
  path = Sys.getenv("GLOBAL_MONITORING_JSON")
)

drive_auth(
  path = Sys.getenv("GLOBAL_MONITORING_JSON")
)

# list all files that can be accessed
all_files <- drive_ls(
  supportsAllDrives = TRUE,
  includeItemsFromAllDrives = TRUE
)

# convenient access to drive file by name
get_gs_file <- function(name) {
  dplyr::filter(all_files, stringr::str_starts(name, !!name))
}

# read the GOogle Sheets
# find the sheet by looking for file name
# that has full stop (no .csv or other extension)
# since Google extensions are not shown in the Drive API
read_gs_file <- function(name, col_types = NULL, ...) {
  ss <- get_gs_file(paste0(name, "$"))
  if (is.null(col_types)) {
    col_types <- get_col_types(name)
  }
  googlesheets4::read_sheet(
    ss = ss,
    sheet = name,
    col_types = col_types,
    ...
  )
}

# since column types are set in stone
# pass col_types explicitly to avoid silent errors due to col type
# (i have seen some due to columns loading as POSIX rather than date )
# also helps to catch if a sheet has been edited or changed without our knowledge
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
    col_types <- "ccccccDDDldddddddddddddddddddd"
  } else if (name == "wrangled_cholera") {
    col_types <- "cDDd"
  } else if (name %in% c("cerf_dashboard_names", "idmc_country_links")) {
    col_types <- "cc"
  } else if (name == "email_recipients") {
    col_types <- "ccll"
  } else {
    NULL
  }
}


# convenient file saver
# find the sheet by looking for file name
# that has full stop (no .csv or other extension)
# since Google extensions are not shown in the Drive API
update_gs_file <- function(df, name) {
  ss <- get_gs_file(paste0(name, "$"))
  googlesheets4::write_sheet(
    data = df,
    ss = ss,
    sheet = name
  )
}
