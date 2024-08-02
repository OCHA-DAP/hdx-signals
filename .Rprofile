source("renv/activate.R")

if (!interactive()) {
  options(warn = 2)
}

if ("box" %in% installed.packages()) {
  box::use(logger[log_threshold])
  log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  log_threshold(log_level)
}

