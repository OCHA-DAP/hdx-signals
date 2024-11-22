#' Validate indicator modules
#'
#' Checks that indicator modules export the correct objects. These include
#' functions to generate data and content: `raw()`, `wrangle()`, `alert()`,
#' `info()` and `summary()`. It also ensures the `indicator_id` is present.
#'
#' @export
validate_indicator <- function(module) {
  # required exports
  req_exports <- c("indicator_id", "raw", "wrangle", "alert", "info", "summary")
  req_missing <- !(req_exports %in% names(module))
  if (any(req_missing)) {
    stop(
      "Not all required objects are exported from ",
      basename(attr(module, "name")),
      ". The following objects are missing: ",
      paste(req_exports[req_missing], collapse = ", "),
      call. = FALSE
    )
  }
}
