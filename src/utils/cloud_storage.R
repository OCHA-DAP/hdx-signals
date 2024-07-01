box::use(
  stringr,
  arrow,
  az = AzureStor,
  glue,
  rlang,
  utils,
  sf,
  tools,
  readr,
  jsonlite,
  dplyr,
  logger
)

box::use(
  src/utils/hs_local,
  src/utils/get_env,
  src/utils/hs_logger
)

hs_logger$configure_logger()

#' Read a Parquet file stored in Microsoft Azure Data Storage container
#'
#' Reads a file from the `hdx-signals` container.
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
#' @param container Container in the Azure storage to read from, either `prod`, `dev`, or `wfp`.
#'
#' @returns Data frame.
#'
#' @export
read_az_file <- function(name, container = c("prod", "dev", "wfp")) {
  container <- get_container(container)
  fileext <- tools$file_ext(name)
  tf <- tempfile(fileext = paste0(".", fileext))

  # wrapping to suppress printing of progress bar
  invisible(
    utils$capture.output(
      az$download_blob(
        container = container,
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

#' Write data frame to Microsoft Azure Data Storage container
#'
#' A convenient file saver that saves the data frame to the specified
#' parquet file. Simply writes out the data frame based on the file extension and
#' uploads that to the MADS container using [AzureStor::upload_blob()].
#' Currently supports Apache Parquet, CSV, GeoJSON, and JSON files.
#'
#' Files written out based on file type:
#'
#' * Apache Parquet: [arrow::write_parquet()]
#' * CSV: [readr::write_csv()]
#' * GeoJSON: [sf::st_write()]
#' * JSON: [jsonlite::write_json]
#'
#' If `hs_local()`, the file is not uploaded to the container.
#'
#' @param df Data frame or simple features to save out.
#' @param name Name of the file to write, including prefix (`input/` or `output/`)
#'     and filetype `.parquet`.
#' @param container Container in the Azure storage to write to, either `prod`, `dev`, or `wfp`.
#'
#' @returns Nothing, but file is written to the `hdx-signals` container.
#'
#' @export
update_az_file <- function(df, name, container = c("prod", "dev", "wfp")) {
  container <- get_container(container)
  fileext <- tools$file_ext(name)
  tf <- tempfile(fileext = paste0(".", fileext))

  switch(fileext,
    csv = readr$write_csv(x = df, file = tf, na = ""),
    parquet = arrow$write_parquet(x = df, sink = tf),
    json = jsonlite$write_json(x = df, path = tf),
    geojson = sf$st_write(obj = df, dsn = tf, quiet = TRUE)
  )

  if (hs_local$hs_local()) {
    logger$log_debug(
      "`update_az_file()` not saving data as `hs_local()` is `TRUE`. ",
      "Set `HS_LOCAL` env variable to `FALSE` if you want the data to be ",
      "saved, but be careful of sending emails or calling the OpenAI API."
    )
    return(invisible(NULL))
  }

  # wrapping to suppress printing of progress bar
  invisible(
    utils$capture.output(
      az$upload_blob(
        container = container,
        src = tf,
        dest = name
      )
    )
  )
}

#' Find file names matching pattern
#'
#' Pulls names from the specified Azure Data Store container and
#' filters those names by the passed patterns, if passed. If `pattern` is `NULL`,
#' then all files in the bucket are returned.
#'
#' @param pattern Pattern to look for. Passed to [stringr::str_detect()]
#' @param container Container in the Azure storage to read from, either `prod`, `dev`, or `wfp`.
#'
#' @export
az_file_detect <- function(pattern = NULL, container = c("prod", "dev", "wfp")) {
  container <- get_container(container)
  # get blob files in container but don't return dirs
  blob_df <- az$list_blobs(container)
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
#' @param container Container in the Azure store to read from, either `prod` or `dev`,
#'     to access the primary `hdx-signals` containers, or `wfp` to read from the WFP
#'     container in `dev`.
#'
#' @returns Correct blob to read and write from
get_container <- function(container = c("prod", "dev", "wfp")) {
  container <- rlang$arg_match(container)
  switch(
    container,
    prod = container_prod(),
    dev = container_dev(),
    wfp = container_wfp()
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

#' Connects to the prod Azure blob container
#'
#' @returns The prod blob container
container_prod <- function() {
  container_endpoint_prod <- az$blob_endpoint(
    endpoint = azure_endpoint_url("blob", "prod"),
    sas = get_env$get_env("DSCI_AZ_SAS_PROD")
  )
  az$blob_container(
    endpoint = container_endpoint_prod,
    name = "hdx-signals"
  )
}

#' Connects to the dev Azure container
#'
#' @returns The dev blob container
container_dev <- function() {
  container_endpoint_dev <- az$blob_endpoint(
    endpoint = azure_endpoint_url("blob", "dev"),
    sas = get_env$get_env("DSCI_AZ_SAS_DEV")
  )
  az$blob_container(
    endpoint = container_endpoint_dev,
    name = "hdx-signals"
  )
}

#' Connects to the Azure blob with WFP data
#'
#' Contains the WFP market monitor data updated by WFP.
#'
#' @returns The WFP blob container
container_wfp <- function() {
  container_endpoint_dev <- az$blob_endpoint(
    endpoint = azure_endpoint_url("blob", "dev"),
    sas = get_env$get_env("DSCI_AZ_SAS_DEV")
  )
  az$blob_container(
    endpoint = container_endpoint_dev,
    name = "wfp"
  )
}
