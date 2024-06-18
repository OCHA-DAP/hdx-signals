box::use(dplyr)
box::use(lubridate)
box::use(purrr)

box::use(../../../../src/utils/ai_summarizer)
box::use(../../../../src/utils/get_prompts)

#' Add campaign info to ACLED conflict data
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts("acled_conflict")

  # get all the event info from the raw data for all the source urls to provide to the user
  df_event_info <- df_wrangled |>
    dplyr$select(
      iso3,
      event_date = date,
      notes
    )

  # now join together and get summarizations
  df_summary <- df_alerts |>
    dplyr$full_join(
      df_event_info,
      by = "iso3"
    ) |>
    dplyr$group_by(iso3, date) |>
    dplyr$filter(
      event_date >= date - lubridate$days(30),
      event_date <= date,
      !is.na(notes)
    ) |>
    dplyr$summarize(
      event_info = paste(notes, collapse = " "),
      plot_title = unique(plot_title),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      summary_long = purrr$map2_chr(
        .x = prompts$long,
        .y = event_info,
        .f = ai_summarizer$ai_summarizer
      ),
      summary_short = ifelse(
        is.na(summary_long) | summary_long == "",
        plot_title,
        purrr$pmap_chr(
          .l = list(
            prompt = prompts$short,
            info = summary_long,
            location = location
          ),
          .f = ai_summarizer$ai_summarizer_without_location
        )
      ),
      summary_source = "ACLED reporting"
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
