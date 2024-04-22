box::use(stringr)
box::use(arrow)
box::use(az = AzureStor)
box::use(glue)
box::use(rlang)
box::use(utils)
box::use(sf)
box::use(tools)

box::use(../utils/gmas_test_run[gmas_test_run])

#' Read a Parquet file stored on Microsoft Azure Data Storage blob
#'
#' Reads a file from the `hdx-signals` bucket.
#' The file is read based on its prefix. Currently, the only support is for
#' Apache Parquet and GeoJSON files, but other support can be added if necessary.
#'
#' Function parsing is done based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()].
#' * GeoJSON: [sf::st_read()]
#'
#' @param name Name of the file to read, including directory prefixes (`input/` or `output/`)
#'     and file extension, such as `.parquet`.
#'
#' @returns Data frame.
#'
#' @export
read_az_file <- function(name) {
  fileext <- tools$file_ext(name)
  tf <- tempfile(fileext = paste0(".", fileext))

  # wrapping to suppress printing of progress bar
  invisible(
    utils$capture.output(
      az$download_blob(
        container = blob,
        src = name,
        dest = tf
      )
    )
  )

  switch(
    fileext,
    parquet = arrow$read_parquet(tf),
    geojson = sf$st_read(tf, quiet = TRUE)
  )
}

#' Write data frame to Microsoft Azure Data Storage
#'
#' A convenient file saver that saves the data frame to the specified
#' parquet file. Simply writes out the data frame based on the file extension and
#' uploads that to the MADS container using [AzureStor::upload_blob()].
#' Currently only supports Apache Parquet files.
#'
#' Files written out based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()]
#'
#' If `gmas_test_run()`, the file is not uploaded to the container
#'
#' @param df Data frame to save out.
#' @param name Name of the file to write, including prefix (`input/` or `output/`)
#'     and filetype `.parquet`.
#' @param
#'
#' @returns Nothing, but file is written to the `hdx-signals` bucket.
#'
#' @export
update_az_file <- function(df, name) {
  tf <- tempfile(fileext = ".parquet")

  arrow$write_parquet(
    x = df,
    sink = tf
  )

  if (gmas_test_run()) {
    message(
      "`update_az_file()` not saving data as `gmas_test_run()` is `TRUE`. ",
      "Set `GMAS_TEST_RUN` env variable to `FALSE` if you want the data to be ",
      "saved, but be careful of sending emails or calling the OpenAI API."
    )
    return(invisible(NULL))
  }

  # wrapping to suppress printing of progress bar
  invisible(
    utils$capture.output(
      az$upload_blob(
        container = blob,
        src = tf,
        dest = name
      )
    )
  )
}

#' Find MADS file names matching pattern
#'
#' Pulls names from the `hdx-signals` bucket and
#' filters those names by the passed patterns, if passed. If `pattern` is `NULL`,
#' then all files in the bucket are returned.
#'
#' @param pattern Pattern to look for. Passed to [stringr::str_detect()]
#'
#' @export
az_file_detect <- function(pattern = NULL) {
  # get blob files but don't return dirs
  blob_df <- az$list_blobs(blob)
  blob_df <- blob_df[!blob_df$isdir,]
  file_names <- blob_df$name
  if (!is.null(pattern)) {
    file_names <- file_names[stringr$str_detect(file_names, pattern)]
  }
  file_names
}

#########################
#### SETUP CONTAINER ####
#########################

#' Get the endpoint URL
#'
#' Currently system uses the blob endpoint, but allows access to the file endpoint
#' as well if necessary. Used to create blob object in `blob_endpoint`.
#'
#' @param service Service to access, either blob (default) or file.
azure_endpoint_url <- function(service = c("blob", "file")) {
  service <- rlang$arg_match(service)
  endpoint <- glue$glue(Sys.getenv("DSCI_AZ_ENDPOINT"))
}

# gets the Dsci blob endpoint using the HDX Signals SAS
blob_endpoint <- az$blob_endpoint(
  endpoint = azure_endpoint_url("blob"),
  sas = Sys.getenv("DSCI_AZ_SAS")
)

# blob object for HDX Signals, used to read and write data
blob <- az$blob_container(
  endpoint = blob_endpoint,
  name = "hdx-signals-mc"
)
