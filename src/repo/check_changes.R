#' Checks that version incrementing has been done correctly in the PR and
#' reflected in CHANGES.md
box::use(stringr)

box::use(../utils/get_signals_version)

main_version <- "https://raw.githubusercontent.com/OCHA-DAP/hdx-signals/main/.signals-version" |>
  readLines() |>
  as.numeric_version()

main_date <- "https://raw.githubusercontent.com/OCHA-DAP/hdx-signals/main/CHANGES.md" |>
  readLines() |>
  paste(collapse = "\n") |>
  stringr$str_extract(
    paste0(
      "(?<=",
      stringr$str_replace_all(
        string = as.character(main_version),
        pattern = "\\.",
        replacement = "\\\\."
      ),
      " \\()(.*)(?=\\)\\\n)"
    )
  ) |>
  as.Date(format = "%d %B %Y")

repo_version <- as.numeric_version(get_signals_version$get_signals_version())

if ((repo_version <= main_version && main_date < Sys.Date()) || main_version > repo_version) {
  stop(
    "Need to increment the version in this branch to be higher than in `main`.",
    call. = FALSE
  )
}

# now check that CHANGES has been updated to have a section for the latest version
branch_changes <- readLines("CHANGES.md") |> paste(collapse = "\n")

if (!stringr$str_detect(
  string = branch_changes,
  pattern = stringr$str_replace_all(
    string = as.character(repo_version),
    pattern = "\\.",
    replacement = "\\\\."
  )
)) {
  stop(
    "No CHANGES.md entry found for '",
    repo_version,
    "'. Please add.",
    call. = FALSE
  )
}
