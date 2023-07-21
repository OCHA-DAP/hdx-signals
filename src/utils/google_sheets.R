library(googlesheets4)
library(googledrive)

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
read_gs_file <- function(name, ...) {
  ss <- get_gs_file(paste0(name, "$"))
  googlesheets4::read_sheet(
    ss = ss,
    sheet = name,
    ...
  )
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
