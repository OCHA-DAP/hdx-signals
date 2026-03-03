box::use(
  dplyr,
  lubridate,
  purrr,
  stringr
)

box::use(
  src/utils/get_prompts,
  src/utils/parse_pdf,
  src/utils/get_manual_info,
  src/utils/python_setup,
  src/signals/track_summary_input
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
    dplyr$filter(indicator_id %in% c("idmc_displacement_conflict", "idmc_displacement_disaster")) |>
    dplyr$distinct(iso3, indicator_id, date) |>
    dplyr$rowwise() |>
    dplyr$mutate(
      manual_info = get_manual_info$get_manual_info(iso3, indicator_id, date)
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
      by = c("iso3", "indicator_id", "date")
    ) |>
    dplyr$filter(
      displacement_end_date >= date - lubridate$days(30),
      displacement_start_date <= date | (Sys.Date() - displacement_start_date <= 90 & Sys.Date() - date <= 90)
    )

  # only extract url info when we can to avoid users
  df_joined$url_info <- NA_character_
  valid_pdfs <- stringr$str_detect(df_joined$event_url, "\\.pdf$")
  df_joined$url_info[valid_pdfs] <- purrr$map_chr(
    .x = df_joined$event_url[valid_pdfs],
    .f = parse_pdf$parse_pdf
  )
  # now group and summarize
  df_summary <- df_joined |>
    dplyr$group_by(iso3, location, date) |>
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
      # ensure valid UTF-8
      overall_info = stringi::stri_encode(overall_info, from = "", to = "UTF-8"),
      summary_long = purrr$pmap_chr(
        .l = list(
          system_prompt = prompts$system,
          user_prompt = prompts$long,
          info = overall_info
        ),
        .f = python_setup$get_summary_r
      ),
      summary_short = purrr$pmap_chr(
        .l = list(
          system_prompt = prompts$system,
          user_prompt = prompts$short,
          info = summary_long,
          location = location
        ),
        .f = python_setup$get_summary_r
      ),
      summary_source = "IDMC analysis and source reports"
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
      indicator_id = indicator_id,
      info = overall_info,
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
