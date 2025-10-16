box::use(
  dplyr,
  lubridate,
  purrr,
  stringr
)

box::use(
  src/utils/ai_summarizer,
  src/utils/get_prompts,
  src/utils/parse_pdf
)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts("idmc_displacement")

  # get all the event info from the raw data for all the source urls to provide to the user
  df_event_info <- df_raw |>
    dplyr$select(
      iso3,
      displacement_start_date,
      displacement_end_date,
      event_info,
      event_url
    )

  df_manual <- df_alerts |>
    dplyr$filter(indicator_id == "idmc_displacement") |>
    dplyr$distinct(iso3, date) |>
    dplyr$rowwise() |>
    dplyr$mutate(
      manual_info = get_manual_info$get_manual_info(iso3, "idmc_displacement", date)
    ) |>
    dplyr$ungroup()

  # now join together and get summarizations
  df_joined <- df_alerts |>
    dplyr$full_join(
      df_event_info,
      by = "iso3"
    ) |>
    dplyr$left_join(
      df_manual,
      by = c("iso3", "date")
    ) |>
    dplyr$group_by(iso3, location, date) |>
    dplyr$filter(
      displacement_end_date >= date - lubridate$days(30),
      # keep recent reports for monitoring
      displacement_start_date <= date | (Sys.Date() - displacement_start_date <= 90 & Sys.Date() - date <= 90)
    )

  # only extract url info when we can to avoid users
  df_joined$url_info <- NA_character_
  valid_pdfs <- stringr$str_detect(df_joined$event_url, "\\.pdf$")
  df_joined$url_info[valid_pdfs] <- purrr$map_chr(
    .x = df_joined$event_url[valid_pdfs],
    .f = parse_pdf$parse_pdf
  )

  df_summary <- df_joined |>
    dplyr$summarize(
      event_info = paste(event_info, collapse = " "),
      url_info = paste(url_info[!is.na(url_info)], collapse = "\n"),
      manual_info = dplyr$first(manual_info, default = NA_character_),
      .groups = "drop"
    ) |>
    dplyr$mutate(
      overall_info = dplyr$case_when(
        url_info == "" ~
          paste(event_info, manual_info, sep = " "),
        .default =
          paste(event_info,
                manual_info,
                ". Here is additional raw text sourced directly from original PDFs --> ",
                sep = " ")
      ),
      summary_long = purrr$map2_chr(
        .x = prompts$long,
        .y = overall_info,
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
      summary_source = "IDMC analysis and source reports"
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
