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
box::use(logger[log_debug])

box::use(../utils/hs_local[hs_local])
box::use(../utils/get_env[get_env])
box::use(../utils/hs_logger)

hs_logger$configure_logger()

#' Read a Parquet file stored on Microsoft Azure Data Storage blob
#'
#' Reads a file from the `hdx-signals` bucket.
#' The file is read based on its prefix in `name`. Currently, the only support is for
#' Apache Parquet, CSV, GeoJSON and GeoJSON files, but other support can be added if necessary.
#'
#' Function parsing is done based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()].
#' * CSV: [readr::read_csv()]
#' * GeoJSON: [sf::st_read()]
#' * JSON: [jsonlite::read_json()]
#'
#' @param name Name of the file to read, including directory prefixes (`input/` or `output/`)
#'     and file extension, such as `.parquet`.
#' @param blob Blob in the Azure storage to read from, either `prod`, `dev`, or `wfp`.
#'
#' @returns Data frame.
#'
#' @export
read_az_file <- function(name, blob = c("prod", "dev", "wfp")) {
  blob <- get_blob(blob)
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
#' If `hs_local()`, the file is not uploaded to the container
#'
#' @param df Data frame or simple features to save out.
#' @param name Name of the file to write, including prefix (`input/` or `output/`)
#'     and filetype `.parquet`.
#' @param blob Blob in the Azure storage to read from, either `prod`, `dev`, or `wfp`.
#'
#' @returns Nothing, but file is written to the `hdx-signals` bucket.
#'
#' @export
update_az_file <- function(df, name, blob = c("prod", "dev", "wfp")) {

  if (hs_local()) {
    log_debug(
      "`update_az_file()` not saving data as `hs_local()` is `TRUE`. ",
      "Set `HS_LOCAL` env variable to `FALSE` if you want the data to be ",
      "saved, but be careful of sending emails or calling the OpenAI API."
    )
    return(invisible(NULL))
  }

  blob <- get_blob(blob)
  fileext <- tools$file_ext(name)
  tf <- tempfile(fileext = paste0(".", fileext))

  switch(fileext,
    csv = readr$write_csv(x = df, file = tf, na = ""),
    parquet = arrow$write_parquet(x = df, sink = tf),
    json = jsonlite$write_json(x = df, path = tf),
    geojson = sf$st_write(obj = df, dsn = tf, quiet = TRUE)
  )


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
#' @param blob Blob in the Azure storage to read from, either `prod`, `dev`, or `wfp`.
#'
#' @export
az_file_detect <- function(pattern = NULL, blob = c("prod", "dev", "wfp")) {
  blob <- get_blob(blob)
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

#' Get blob container
#'
#' @param blob Blob in the Azure store to read from, either `prod` or `dev`,
#'     to access the primary `hdx-signals` blobs, or `wfp` to read from the WFP
#'     blob in dev.
#'
#' @returns Correct blob to read and write from
get_blob <- function(blob = c("prod", "dev", "wfp")) {
  blob <- rlang$arg_match(blob)
  switch(
    blob,
    prod = blob_prod(),
    dev = blob_dev(),
    wfp = blob_wfp()
  )
}

#' Get the endpoint URL
#'
#' Currently system uses the blob endpoint, but allows access to the file endpoint
#' as well if necessary. Used to create blob object in `blob_endpoint`.
#'
#' @param service Service to access, either `blob` (default) or `file.`
#' @param stage Store to access, either `prod` (default) or `dev`. `dev`
#'     is used because some exernal services can only read from `dev` currently,
#'     so need to store the output CSV for HDX on `dev`, and some external data
#'     read for input is on `dev`.
azure_endpoint_url <- function(service = c("blob", "file"), stage = c("prod", "dev")) {
  service <- rlang$arg_match(service)
  stage <- rlang$arg_match(stage)
  # service and stage injected into endpoint string using `{glue}`
  glue$glue("https://imb0chd0{stage}.{service}.core.windows.net/")
}

#' Builds the path of the signals.parquet files
#'
#' @param indicator_id ID of the indicator
#' @param dry_run Whether looking for the test file or not
#'
#' @returns String path
#'
#' @export
signals_path <- function(indicator_id, dry_run) {
  paste0(
    "output/",
    indicator_id,
    if (dry_run) "/test" else "",
    "/signals.parquet"
  )
}

#' Connects to the prod Azure blob
#'
#' @returns The prod blob container
blob_prod <- function() {
  blob_endpoint_prod <- az$blob_endpoint(
    endpoint = azure_endpoint_url("blob", "prod"),
    sas = get_env("DSCI_AZ_SAS_PROD")
  )
  az$blob_container(
    endpoint = blob_endpoint_prod,
    name = "hdx-signals"
  )
}

#' Connects to the dev Azure blob
#'
#' @returns The dev blob container
blob_dev <- function() {
  blob_endpoint_dev <- az$blob_endpoint(
    endpoint = azure_endpoint_url("blob", "dev"),
    sas = get_env("DSCI_AZ_SAS_DEV")
  )
  az$blob_container(
    endpoint = blob_endpoint_dev,
    name = "hdx-signals"
  )
}

#' Connects to the Azure blob with WFP data
#'
#' Contains the WFP market monitor data updated by WFP.
#'
#' @returns The WFP blob container
blob_wfp <- function() {
  blob_endpoint_dev <- az$blob_endpoint(
    endpoint = azure_endpoint_url("blob", "dev"),
    sas = get_env("DSCI_AZ_SAS_DEV")
  )
  az$blob_container(
    endpoint = blob_endpoint_dev,
    name = "wfp"
  )
}
