source("renv/activate.R")
box::use(logger[log_threshold])

if (!interactive()) {
  options(warn = 2)
}

log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")
log_threshold(log_level)
