box::use(
  dplyr,
  purrr
)

box::use(
  src/utils/ai_summarizer,
  src/utils/get_prompts,
  src/utils/get_manual_info,
  src/signals/track_summary_input,
  src/utils/hs_dry_run,
)

jrc_agricultural_hotspots <- "jrc_agricultural_hotspots"

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts(jrc_agricultural_hotspots)

  df_manual <- df_alerts |>
    dplyr$filter(indicator_id == jrc_agricultural_hotspots) |>
    dplyr$distinct(iso3, date) |>
    dplyr$rowwise() |>
    dplyr$mutate(
      manual_info = get_manual_info$get_manual_info(iso3, jrc_agricultural_hotspots, date)
    ) |>
    dplyr$ungroup()


  df_summary <- df_alerts |>
    dplyr$select(-comment) |>
    dplyr$full_join(
      dplyr$select(df_wrangled, iso3, comment_date = date, date_label, comment),
      by = "iso3",
      relationship = "many-to-many"
    ) |>
    dplyr$filter(
      comment_date <= date,
      date - comment_date <= 190
    ) |>
    dplyr$left_join(
      df_manual,
      by = c("iso3", "date")
    ) |>
    dplyr$group_by(iso3, location, date) |>
    dplyr$summarize(
      info = paste(date_label, comment, sep = ": ", collapse = "\n"),
      manual_info = dplyr$first(manual_info, default = NA_character_),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      info = paste(
        info, manual_info, sep = "\n"
      ),
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
  result <- df_alerts |>
    dplyr$left_join(
      df_summary,
      by = c("iso3", "location", "date")
    )

  # Track only the signals that were actually generated
  tracking_data <- result |>
    dplyr$transmute(
      location_iso3 = iso3,
      date_generated = date,
      indicator_id = jrc_agricultural_hotspots,
      info = info,
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
