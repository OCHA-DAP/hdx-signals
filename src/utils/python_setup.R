box::use(reticulate)

# Decide Python dynamically
python_path <- Sys.getenv("PYTHON_BIN", unset = NA)

if (!is.na(python_path) && nzchar(python_path)) {
  # GitHub Actions (or any CI)
  reticulate$use_python(python_path, required = TRUE)
} else {
  python_path <- Sys.getenv("PYTHON_PATH", unset = NA)
  # Local development
  reticulate$use_python(python_path, required = TRUE
  )
}

# Load Python functions
reticulate$source_python("src/python_scripts/azure_script.py")

#' @export
get_summary_r <- function(system_prompt, user_prompt, info, location = NULL) {
  if (hs_local$hs_local()) {
    logger$log_debug(
      "`get_summary_r()` returning static output as `hs_local()` is `TRUE`. ",
      "Set `HS_LOCAL` env variable to `FALSE` if you want `get_summary_r()` ",
      "to ping the OpenAI API via Azure, but be wary of saving data and emailing."
    )

    "Test output."
  } else {
    reticulate::py$get_summary(
      system_prompt = system_prompt,
      user_prompt = user_prompt,
      info = info,
      location = location
    )
  }
}
