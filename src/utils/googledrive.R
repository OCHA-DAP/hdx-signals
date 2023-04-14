library(googledrive)

# authorize drive access
drive_auth(
  path = Sys.getenv("GLOBAL_MONITORING_JSON")
)

# list all files that can be accessed
all_files <- drive_ls(
  supportsAllDrives = TRUE,
  includeItemsFromAllDrives = TRUE
)

# convenient access to drive file by name
get_drive_file <- function(name) {
  dplyr::filter(all_files, stringr::str_starts(name, !!name))
}

# convenient file saver
update_drive_file <- function(df, local_path, drive_file) {
  write_csv(df, local_path, na = "")
  drive_update(drive_file, local_path)
}
