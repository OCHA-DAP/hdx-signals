box::use(
  readr,
  utils,
  dplyr,
  curl
)

box::use(src/utils/location_codes)

#' Download a file, retrying on transient errors
#'
#' The JRC server intermittently returns 403 to GitHub runners, so the request
#' uses a browser-like user agent and is retried with increasing waits.
#'
#' @param url URL to download.
#' @param destfile Path to write the file to.
#' @param max_attempts Maximum number of download attempts.
download_zip <- function(url, destfile, max_attempts = 4) {
  handle <- curl$new_handle(
    useragent = paste(
      "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36",
      "(KHTML, like Gecko) Chrome/120.0 Safari/537.36"
    )
  )
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      curl$curl_download(url, destfile, quiet = TRUE, handle = handle),
      error = function(e) e
    )
    if (!inherits(result, "error")) {
      return(invisible(destfile))
    }
    if (attempt == max_attempts) {
      stop(result)
    }
    message("Download attempt ", attempt, " failed: ", conditionMessage(result))
    Sys.sleep(30 * attempt)
  }
}

#' Download raw JRC ASAP drought data
#'
#' Downloads raw JRC ASAP drought data from HDX.
#'
#' @export
raw <- function() {
  zf <- tempfile(fileext = ".zip")
  download_zip(
    url = "https://agricultural-production-hotspots.ec.europa.eu/files/hotspots_html_ts.zip",
    destfile = zf
  )

  utils$unzip(
    zipfile = zf,
    exdir = td <- tempdir()
  )

  readr$read_delim(
    file = file.path(
      td, "hotspots_html_ts.csv"
    ),
    col_types = readr$cols(),
    delim = ";"
  ) |>
    dplyr$mutate(
      iso3 = location_codes$names_to_iso3(asap0_name)
    )
}
