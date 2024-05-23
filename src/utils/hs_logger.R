box::use(logger[log_threshold])

#' Sets up the logging environment
#'
#' @export
configure_logger <- function() {
  log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  log_threshold(log_level)
}

#' Reusable logging for monitoring script setup
#'
#' @export
monitoring_log_setup <- function(indicator_id) {
  log_info(paste0("Checking ", indicator_id, "..."))
  log_debug(paste0("GMAS_TEST_RUN = ", Sys.getenv("GMAS_TEST_RUN")))
  log_debug(paste0("TEST = ", Sys.getenv("TEST")))
  log_debug(paste0("FIRST_RUN = ", Sys.getenv("FIRST_RUN")))
}
