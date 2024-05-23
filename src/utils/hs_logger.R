box::use(logger[log_threshold])

#' Sets up the logging environment
#'
#' @export
configure_logger <- function() {
  log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  log_threshold(log_level)
}
