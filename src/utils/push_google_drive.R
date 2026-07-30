box::use(
  googledrive,
  readr,
  logger
)

box::use(
  src/utils/get_env,
  src/utils/hs_local,
  src/utils/temp_file
)

#' ID of the Google Drive folder that receives the latest signals export
gdrive_folder_id <- "1R5wbgrKJLOfkGvEUqSeSh17Rm7ovd2Mw"

#' Authenticate with Google Drive
#'
#' Authenticates {googledrive} non-interactively using a service account key.
#' `HS_GDRIVE_SERVICE_ACCOUNT` holds the service account JSON directly (not a
#' file path), but it's written out to a session tempfile rather than passed
#' to [googledrive::drive_auth()] as a raw string, because environments that
#' shell out to pass env vars to R (e.g. Windows `.bat` wrappers) can silently
#' truncate a multi-line string value. The tempfile lives in `.temp_dir`,
#' which is cleared out on every module load (see `temp_file.R`).
gdrive_auth <- function() {
  key_path <- temp_file$temp_file(".json")
  writeLines(get_env$get_env("HS_GDRIVE_SERVICE_ACCOUNT"), key_path)
  googledrive$drive_auth(path = key_path)
}

#' Write a data frame to Google Drive as a CSV
#'
#' Writes `df` to a temporary CSV and uploads it to the `gdrive_folder_id`
#' Google Drive folder. If a file called `name` already exists in that
#' folder, its content is replaced in place so the same file/link is reused
#' rather than creating a duplicate each time.
#'
#' If `hs_local()` is `TRUE`, the upload is skipped, matching the behaviour of
#' `cloud_storage$update_az_file()`.
#'
#' @param df Data frame to upload.
#' @param name File name, including the `.csv` extension, to give the file on
#'     Google Drive.
#'
#' @returns Nothing, but the file is written to the Google Drive folder.
#'
#' @export
push_google_drive <- function(df, name) {
  if (hs_local$hs_local()) {
    logger$log_debug(
      "`push_google_drive()` not saving data as `hs_local()` is `TRUE`. ",
      "Set `HS_LOCAL` env variable to `FALSE` if you want the data to be ",
      "saved to Google Drive."
    )
    return(invisible(NULL))
  }

  gdrive_auth()

  tf <- temp_file$temp_file(".csv")
  readr$write_csv(x = df, file = tf, na = "")

  folder_id <- googledrive$as_id(gdrive_folder_id)
  existing_file <- googledrive$drive_ls(
    path = folder_id,
    pattern = paste0("^", name, "$")
  )

  if (nrow(existing_file) > 0) {
    googledrive$drive_update(
      file = googledrive$as_id(existing_file$id[1]),
      media = tf
    )
  } else {
    googledrive$drive_upload(
      media = tf,
      path = folder_id,
      name = name
    )
  }
}
