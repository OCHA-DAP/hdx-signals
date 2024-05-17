box::use(stringr)
box::use(arrow)
box::use(az = AzureStor)
box::use(glue)
box::use(rlang)
box::use(utils)
box::use(sf)
box::use(tools)
box::use(readr)
box::use(jsonlite)
box::use(dplyr)

box::use(../utils/gmas_test_run[gmas_test_run])

#' Read a Parquet file stored on Microsoft Azure Data Storage blob
#'
#' Reads a file from the `hdx-signals` bucket.
#' Th"e file is read based on its prefix. Currently, the only support is for
#' Apache Parquet and GeoJSON files, but other support can be added if necessary.
#'
#' Function parsing is done based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()].
#' * GeoJSON: [sf::st_read()]
#' * CSV: [readr::read_csv()]
#'
#' @param name Name of the file to read, including directory prefixes (`input/` or `output/`)
#'     and file extension, such as `.parquet`.
#' @param stage Stage in the Azure storage to read from, either `prod` or `dev`.
#'
#' @returns Data frame.
#'
#' @export
read_az_file <- function(name, stage = c("prod", "dev")) {
  blob <- stage_to_blob(stage)
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

  switch(fileext,
    parquet = arrow$read_parquet(tf),
    geojson = sf$st_read(tf, quiet = TRUE),
    json = dplyr$as_tibble(jsonlite$read_json(tf, simplifyVector = TRUE)),
    csv = readr$read_csv(tf, col_types = readr$cols())
  )
}

#' Write data frame to Microsoft Azure Data Storage
#'
#' A convenient file saver that saves the data frame to the specified
#' parquet file. Simply writes out the data frame based on the file extension and
#' uploads that to the MADS container using [AzureStor::upload_blob()].
#' Currently supports Apache Parquet, GeoJSON, and CSV files.
#'
#' Files written out based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()]
#' * GeoJSON: [sf::st_write()]
#' * CSV: [readr::write_csv()]
#'
#' If `gmas_test_run()`, the file is not uploaded to the container
#'
#' @param df Data frame or simple features to save out.
#' @param name Name of the file to write, including prefix (`input/` or `output/`)
#'     and filetype `.parquet`.
#' @param stage Stage in the Azure storage to read from, either `prod` or `dev`.
#'
#' @returns Nothing, but file is written to the `hdx-signals` bucket.
#'
#' @export
update_az_file <- function(df, name, stage = c("prod", "dev")) {
  blob <- stage_to_blob(stage)
  fileext <- tools$file_ext(name)
  tf <- tempfile(fileext = paste0(".", fileext))

  switch(fileext,
    csv = readr$write_csv(x = df, file = tf),
    parquet = arrow$write_parquet(x = df, sink = tf),
    json = jsonlite$write_json(x = df, path = tf),
    geojson = sf$st_write(obj = df, dsn = tf, quiet = TRUE)
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
#' @param stage Stage in the Azure storage to read from, either `prod` or `dev`.
#'
#' @export
az_file_detect <- function(pattern = NULL, stage = c("prod", "dev")) {
  blob <- stage_to_blob(stage)
  # get blob files but don't return dirs
  blob_df <- az$list_blobs(blob)
  blob_df <- blob_df[!blob_df$isdir, ]
  file_names <- blob_df$name
  if (!is.null(pattern)) {
    file_names <- file_names[stringr$str_detect(file_names, pattern)]
  }
  file_names
}

#########################
#### SETUP CONTAINER ####
#########################

#' Stage to blob
#'
#' Checks the stage argument and ensures that the correct blob is returned.
#'
#' @param stage Stage in the Azure storage to read from, either `prod` or `dev`.
#'
#' @returns Correct blob to read and write from
stage_to_blob <- function(stage = c("prod", "dev")) {
  stage <- rlang$arg_match(stage)
  if (stage == "prod") {
    blob_prod
  } else {
    blob_dev
  }
}

#' Get the endpoint URL
#'
#' Currently system uses the blob endpoint, but allows access to the file endpoint
#' as well if necessary. Used to create blob object in `blob_endpoint`.
#'
#' @param service Service to access, either `blob` (default) or `file.`
#' @param stage Stage to access, either `prod` (default) or `dev`. Used because
#'     some external services can only read from `dev` currently, so need to
#'     store the output CSV for HDX on `dev`.
azure_endpoint_url <- function(service = c("blob", "file"), stage = c("prod", "dev")) {
  service <- rlang$arg_match(service)
  stage <- rlang$arg_match(stage)
  # service and stage injected into endpoint string using `{glue}`
  dsci_az_endpoint = "https://imb0chd0{stage}.{service}.core.windows.net/"
  glue$glue(dsci_az_endpoint)
}

# gets the Dsci blob endpoints using the HDX Signals SAS
blob_endpoint_dev <- az$blob_endpoint(
  endpoint = azure_endpoint_url("blob", "dev"),
  sas = Sys.getenv("DSCI_AZ_SAS_DEV")
)

blob_endpoint_prod <- az$blob_endpoint(
  endpoint = azure_endpoint_url("blob", "prod"),
  sas = Sys.getenv("DSCI_AZ_SAS_PROD")
)


# blob object for HDX Signals, used to read and write data
blob_dev <- az$blob_container(
  endpoint = blob_endpoint_dev,
  name = "hdx-signals"
)

# blob object for HDX Signals, used to read and write data
blob_prod <- az$blob_container(
  endpoint = blob_endpoint_prod,
  name = "hdx-signals-mc"
)
