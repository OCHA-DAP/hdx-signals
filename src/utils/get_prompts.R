box::use(
  glue,
  purrr,
  stats
)

#' Get prompts for `indicator_id`
#'
#' Get prompts for `indicator_id` stored in the indicator folder.
#'
#' @param indicator_id Indicator ID
#' @param prompts Character string of extra prompts to read in from the indicator
#'     folder. `c("a", "b")` would refer to `a.txt` and `b.txt`, for instance.
#'     Defaults to `short` and `long`, what most indicators use.
#'
#' @returns A list all prompts found in `prompts`.
#'
#' @export
get_prompts <- function(indicator_id, prompts = c("short", "long")) {
  fp <- file.path(
    "src",
    "indicators",
    indicator_id,
    "prompts"
  )

  purrr$map(
    .x = prompts,
    .f = \(x) {
      readLines(
        file.path(
          fp,
          glue$glue("{x}.txt")
        )
      ) |>
        paste(collapse = " ")
    }
  ) |>
    stats$setNames(prompts)
}
