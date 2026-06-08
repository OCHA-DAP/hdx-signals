box::use(logger)

#' Sets up the logging environment
#'
#' @export
configure_logger <- function() {
  log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  logger$log_threshold(log_level)
}

#' Reusable logging for monitoring script setup
#'
#' @export
monitoring_log_setup <- function(indicator_id) {
  configure_logger()
  logger$log_info(paste0("Checking ", indicator_id, "..."))
  logger$log_debug(paste0("HS_LOCAL = ", Sys.getenv("HS_LOCAL")))
  logger$log_debug(paste0("HS_DRY_RUN = ", Sys.getenv("HS_DRY_RUN")))
  logger$log_debug(paste0("HS_FIRST_RUN = ", Sys.getenv("HS_FIRST_RUN")))
}
