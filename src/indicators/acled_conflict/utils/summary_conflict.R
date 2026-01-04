box::use(
  dplyr,
  lubridate,
  purrr,
  reticulate
)

box::use(
  src/utils/ai_summarizer,
  src/utils/get_prompts,
  src/utils/get_manual_info,
  src/utils/python_setup,
  src/signals/track_summary_input
)

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

  # get manual contextual info
  df_manual <- df_alerts |>
    dplyr$filter(indicator_id == "acled_conflict") |>
    dplyr$distinct(iso3, date) |>
    dplyr$rowwise() |>
    dplyr$mutate(
      manual_info = get_manual_info$get_manual_info(iso3, "acled_conflict", date)
    ) |>
    dplyr$ungroup()

  # now join together and get summarizations
  df_summary <- df_alerts |>
    dplyr$full_join(
      df_event_info,
      by = "iso3"
    ) |>
    dplyr$left_join(
      df_manual,
      by = c("iso3", "date")
    ) |>
    dplyr$filter(
      event_date >= date - lubridate$days(30),
      event_date <= date,
      !is.na(notes)
    ) |>
    dplyr$group_by(iso3, location, date) |>
    dplyr$summarize(
      event_info = paste(notes, collapse = " "),
      manual_info = dplyr$first(manual_info, default = NA_character_),
      plot_title = unique(plot_title),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      # Combine event_info with manual_info if present
      event_info = paste(event_info, manual_info, sep = " "),
      summary_long =purrr$pmap_chr(
        .l = list(
          system_prompt = prompts$system,
          user_prompt = prompts$long,
          info = event_info
          ),
        .f = python_setup$get_summary_r),
      summary_short = ifelse(
        is.na(summary_long) | summary_long == "",
        plot_title,
        purrr$pmap_chr(
          .l = list(
            system_prompt = prompts$system,
            user_prompt = prompts$short,
            location = location,
            info = summary_long
            ),
            .f = python_setup$get_summary_r
          )
        ),
      summary_source = "ACLED reporting"
    )

  # ensuring the output matches the original input
  result <- df_alerts |>
    dplyr$left_join(
      df_summary,
      by = c("iso3", "location", "date")
    )

  # track summarizer input
  tracking_data <- result |>
    dplyr$transmute(
      location_iso3 = iso3,
      date_generated = date,
      indicator_id = "acled_conflict",
      info = event_info,
      manual_info = manual_info,
      use_manual_info = !is.na(manual_info),
      summary_long = summary_long,
      summary_short = summary_short,
      summary_source = summary_source
    )

  track_summary_input$append_tracking_data(tracking_data)

  result |>
    dplyr$select(
      summary_long,
      summary_short,
      summary_source
    )
}
