source("renv/activate.R")

# ensure warnings error out runs on GitHub Actions
if (!interactive()) {
  options(warn = 2)
}

# box module sourcing from the working directory, helps with linting and other issues
# https://github.com/Appsilon/box.linters/issues/110#issuecomment-2182002089
options(box.path = getwd())

if ("box" %in% installed.packages()) {
  box::use(logger[log_threshold])
  log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  log_threshold(log_level)
}
