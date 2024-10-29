box::use(here)

#' Temporary file for use across modules
#'
#' Used to allow us to view preview campaign HTML files with images, because they
#' can only be viewed if they are stored in the same directory as the HTML file.
#' Simply creates tempfiles in the `.tempdir` folder.
#'
#' This folder is used rather than `tempdir()` to ensure it exists across
#' module runs. Issues were encountered with `tempdir()` disappearing between
#' module calls so we couldn't place the HTML and images in the same folder.
#'
#' @param fileext File extension, passed to `temp.file()`.
#'
#' @examples
#' temp_file(".csv")
#'
#' @return Filepath
#'
#' @export
temp_file <- function(fileext = "") {
  tempfile(
    tmpdir = temp_dir,
    fileext = fileext
  )
}

# ensure directory is created and empty when this module is loaded
.on_load <- function(ns) {
  temp_dir <- here$here(".temp_dir")
  dir.create(temp_dir, showWarnings = FALSE)
  unlink(paste0(temp_dir, "/*"), recursive = TRUE)
}
