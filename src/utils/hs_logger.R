box::use(logger[log_threshold, log_info, log_debug])

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
  log_debug(paste0("HS_LOCAL = ", Sys.getenv("HS_LOCAL")))
  log_debug(paste0("HS_TEST = ", Sys.getenv("HS_TEST")))
  log_debug(paste0("FIRST_RUN = ", Sys.getenv("FIRST_RUN")))
}
