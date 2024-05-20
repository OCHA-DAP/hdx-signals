box::use(dplyr)
box::use(purrr)

box::use(../../../../src/utils/ai_summarizer)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  df_summary <- df_alerts |>
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
      prompt_long = paste(
        "Below, we will provide you information on crop-related drought conditions",
        "over a period of time up to 6 months. Data is presented with the earliest",
        "information first, formatted as 'Month 1: {information}\n Month 2:",
        "{information}' and so forth. We are most interested in conditions in the",
        "latest month presented, which will come last. Please summarize the situation",
        "now, using information from previous months to contextualize the recent",
        "past in the country. Output your summarization as a short paragraph of",
        "3 to 4 sentences at most. Focus on summarizing information presented.",
        "The information for summarization follows --> "
      ),
      prompt_short = paste(
        "Please condense the following information into a single 10 word line,",
        "similar to text you might see on a news ticker. Outputs could look like",
        "the following 2 examples:",
        "'Tropical storm threatens crop harvest, exacerbating socio-economic driven",
        "food insecurity' or 'Rainfall patterns have improved, but poor",
        "vegetation conditions conntinue in the south'.",
        "Expect the reader to have no context, but this is",
        "intended to capture their attention, so keep the messaging simple, clear",
        "and punchy. Do not include country names in the output.",
        "Use only the information following in your summary: --> "
      ),
      summary_long = purrr$map2_chr(
        .x = prompt_long,
        .y = info,
        .f = ai_summarizer$ai_summarizer
      ),
      summary_short = purrr$map2_chr(
        .x = prompt_short,
        .y = summary_long,
        .f = ai_summarizer$ai_summarizer
      )
    )

  # ensuring the output matches the original input
  df_alerts |>
    dplyr$left_join(
      df_summary,
      by = c("iso3", "date")
    ) |>
    dplyr$select(
      summary_long,
      summary_short
    )
}
