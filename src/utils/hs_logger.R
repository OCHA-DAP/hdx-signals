box::use(logger[log_info, log_debug])

#' Reusable logging for monitoring script setup
#'
#' @export
monitoring_log_setup <- function(indicator_id) {
  log_info(paste0("Checking ", indicator_id, "..."))
  log_debug(paste0("HS_LOCAL = ", Sys.getenv("HS_LOCAL")))
  log_debug(paste0("HS_DRY_RUN = ", Sys.getenv("HS_DRY_RUN")))
  log_debug(paste0("HS_FIRST_RUN = ", Sys.getenv("HS_FIRST_RUN")))
}
