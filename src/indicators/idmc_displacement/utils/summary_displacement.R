box::use(dplyr)
box::use(lubridate)
box::use(purrr)

box::use(../../../../src/utils/ai_summarizer)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  # get all the event info from the raw data for all the source urls to provide to the user
  df_event_info <- df_raw |>
    dplyr$select(
      iso3,
      displacement_start_date,
      displacement_end_date,
      event_info
    )

  # now join together and get summarizations
  df_summary <- df_alerts |>
    dplyr$full_join(
      df_event_info,
      by = "iso3"
    ) |>
    dplyr$group_by(iso3, date) |>
    dplyr$filter(
      displacement_end_date >= date - lubridate$days(30),
      # keep recent reports for monitoring
      displacement_start_date <= date | (Sys.Date() - displacement_start_date <= 90 & Sys.Date() - date <= 90)
    ) |>
    dplyr$summarize(
      event_info = paste(event_info, collapse = " "),
      prompt_long = paste(
        "In a short paragraph, please summarize the main reasons for displacement.",
        "Avoid providing specific numbers or dates, just provide the general",
        "reasons behind the displacement and other key qualitative information.",
        "Only use the information below:\n\n"
      ),
      prompt_short = paste(
        "Please condense the following information into a single 10 word line,",
        "similar to text you might see on a news ticker. Outputs could look like",
        "the following 2 examples:",
        "'Armed attacks in the capital force residents to flee to the countryside' or",
        "'Instability due to gang activity drives displacement across the country'.",
        "Expect the reader to have no context, but this is",
        "intended to capture their attention, so keep the messaging simple, clear",
        "and punchy. Use only the information below in your summary:\n\n"
      ),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      summary_long = purrr$map2_chr(
        .x = prompt_long,
        .y = event_info,
        .f = ai_summarizer$ai_summarizer
      ),
      summary_short = purrr$map2_chr(
        .x = prompt_short,
        .y = summary_long,
        .f = ai_summarizer$ai_summarizer
      ),
      summary_source = "IDMC analysis and source reports"
    )

  # ensuring the output matches the original input
  df_alerts |>
    dplyr$left_join(
      df_summary,
      by = c("iso3", "date")
    ) |>
    dplyr$select(
      summary_long,
      summary_short,
      summary_source
    )
}
