box::use(dplyr)

#' Add summary to cholera alerts
#'
#' @returns Data frame with summaries
#'
#' @export
summary <- function(df, df_wrangled, df_raw) {
  df |>
    dplyr$transmute(
      summary_long = NA_character_,
      summary_short = title,
      summary_source = NA_character_
    )
}
