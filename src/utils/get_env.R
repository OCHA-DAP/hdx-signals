box::use(logger[log_error])


#' Gets an environment variable value, returning an error if it is not set
#'
#' @param env_var_name Name of environment variable
#'
#' @returns Value of environment variable, unless it is not set
#' @export
get_env <- function(env_var_name, output = TRUE) {
  val <- Sys.getenv(env_var_name)
  if (!nzchar(val)) {
    error_message <- sprintf("Environment variable '%s' is empty or not set.", env_var_name)
    log_error(error_message)
    stop(call. = FALSE)
  } else if (output) {
    val
  }
}
