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
get_summary_r <- function(system_prompt, user_prompt, info, location=NULL) {
  reticulate::py$get_summary(
    system_prompt = system_prompt,
    user_prompt   = user_prompt,
    info          = info,
    location = location
  )
}
