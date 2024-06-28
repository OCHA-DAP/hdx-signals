box::use(dplyr)
box::use(purrr)

box::use(src/utils/ai_summarizer)
box::use(src/utils/get_prompts)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts("jrc_agricultural_hotspots")

  df_summary <- df_alerts |>
    dplyr$select(
      -comment
    ) |>
    dplyr$full_join(
      dplyr$select(df_wrangled, iso3, comment_date = date, date_label, comment),
      by = "iso3",
      relationship = "many-to-many"
    ) |>
    dplyr$filter(
      comment_date <= date,
      date - comment_date <= 190
    ) |>
    dplyr$group_by(
      iso3,
      location,
      date
    ) |>
    dplyr$summarize(
      info = paste(
        date_label,
        comment,
        sep = ": ",
        collapse = "\n"
      ),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      summary_long = purrr$map2_chr(
        .x = prompts$long,
        .y = info,
        .f = ai_summarizer$ai_summarizer
      ),
      summary_short = purrr$pmap_chr(
        .l = list(
          prompt = prompts$short,
          info = summary_long,
          location = location
        ),
        .f = ai_summarizer$ai_summarizer_without_location
      ),
      summary_source = "the JRC-ASAP system"
    )

  # ensuring the output matches the original input
  df_alerts |>
    dplyr$left_join(
      df_summary,
      by = c("iso3", "location", "date")
    ) |>
    dplyr$select(
      summary_long,
      summary_short,
      summary_source
    )
}
