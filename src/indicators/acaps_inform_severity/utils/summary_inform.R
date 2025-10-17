box::use(
  httr2,
  dplyr,
  lubridate,
  tidyr,
  purrr
)
box::use(
  src/utils/ai_summarizer,
  src/utils/get_prompts,
  src/utils/get_env,
  src/utils/get_manual_info
)

#' Add campaign info to ACAPS inform data
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts("acaps_inform_severity")

  #Call api for each alert to get Justification
  df_wrangled$date <- as.Date(df_wrangled$date)
  df_alerts$date <- as.Date(df_alerts$date)
  df_wrangled_national <- df_wrangled |>
    dplyr$filter(country_level == "Yes") |>
    dplyr$rename(value = inform_severity_index)
  df_wrangled_alerting <- dplyr$right_join(df_wrangled_national, df_alerts, by = c("iso3", "date", "value"))

  df_justification <- dplyr$tibble(
    iso3 = character(),
    country = character(),
    regions = character(),
    crisis_id = character(),
    crisis_name = character(),
    indicator = character(),
    figure = numeric(),
    justification = character(),
    internal_date = as.Date(character()),
    date = as.Date(character())
  )

  df_daily_monitoring <- dplyr$tibble(
    iso3 = character(),
    country = character(),
    latest_developments = character(),
    internal_date = as.Date(character()),
    date = as.Date(character())
  )

  df_country <- dplyr$tibble(
    iso3 = character(),
    country = character(),
    regions = character(),
    crises = character(),
    overview = character(),
    date_request = as.Date(character())
  )

  df_manual <- dplyr$tibble(
    iso3 = character(),
    date = as.Date(character()),
    manual_info = character()
  )

  for (ii in seq_len(nrow(df_wrangled_alerting))){
    date_param <- df_wrangled_alerting[[ii, "date"]]
    iso3_param <- df_wrangled_alerting[[ii, "iso3"]]
    country_param <- df_wrangled_alerting[[ii, "country"]]

    df_justification_single <- get_inform_justification(iso3_param, date_param, country_param)
    df_country_single <- get_country_info(iso3_param, date_param)
    df_daily_monitoring_single <- get_daily_monitoring(date_param, iso3_param)
    df_manual_single <- get_manual_info$get_manual_info(iso3_param, "acaps_inform_severity", date_param)

    df_justification <- dplyr$bind_rows(df_justification, df_justification_single)
    df_country <- dplyr$bind_rows(df_country, df_country_single)
    df_daily_monitoring <- dplyr$bind_rows(df_daily_monitoring, df_daily_monitoring_single)
    df_manual <- dplyr$bind_rows(df_manual, df_manual_single)
  }

  df_justification <- df_justification |>
    dplyr$group_by(iso3, crisis_id, indicator) |>
    dplyr$mutate(
      max_date = max(internal_date),
      first_day_of_max_month = lubridate$floor_date(max_date, "month")
    ) |>
    dplyr$filter(internal_date >= first_day_of_max_month) |>
    dplyr$select(-max_date, -first_day_of_max_month)
  id_cols <- c("iso3", "date", "crisis_id", "country")

  df_justification <- df_justification |>
    dplyr$mutate(text = paste(indicator, figure, justification, sep = ": ")) |>
    tidyr$pivot_wider(id_cols = id_cols, names_from = indicator, values_from = text) |>
    dplyr$ungroup() |>
    dplyr$mutate(text = do.call(
      paste,
      c(dplyr$across(-dplyr$all_of(id_cols)), sep = " ")
    )) |>
    dplyr$group_by(iso3, date) |>
    dplyr$summarize(text = paste(text, collapse = " "), .groups = "drop") |>
    dplyr$select(c("iso3", "date", "text"))

  df_daily_monitoring <- df_daily_monitoring |>
    dplyr$group_by(iso3) |>
    dplyr$filter(internal_date == max(internal_date))
  df_daily_monitoring <- df_daily_monitoring[!duplicated(df_daily_monitoring), ]

  df_country <- df_country[!duplicated(df_country), ] |>
    dplyr$mutate(overview = paste("date request:", date_request, ".", overview, sep = " ")) |>
    dplyr$rename(crisis_id = crises, date = date_request)

  df_country <- df_country |>
    dplyr$distinct(iso3, date, overview, .keep_all = TRUE) |>
    dplyr$group_by(iso3, date) |>
    dplyr$summarize(overview = paste(overview, collapse = " "), .groups = "drop") |>
    dplyr$select(c("iso3", "date", "overview"))

  df_text <- df_alerts |>
    dplyr$left_join(df_justification, by = c("iso3", "date")) |>
    dplyr$select(c("iso3", "date", "text", "location"))

  df_text <- df_text |>
    dplyr$left_join(df_daily_monitoring, by = c("iso3", "date")) |>
    dplyr$select(c("iso3", "date", "text", "latest_developments", "location"))

  df_text <- df_text |>
    dplyr$left_join(df_country, by = c("iso3", "date")) |>
    dplyr$select(c("iso3", "date", "text", "latest_developments", "overview", "location"))

  df_text <- df_text |>
    dplyr$left_join(df_manual, by = c("iso3", "date")) |>
    dplyr$select(c("iso3",
                   "date",
                   "text",
                   "latest_developments",
                   "overview",
                   "manual_info",
                   "manual_source",
                   "location"))

  df_text <- df_text |>
    dplyr$mutate(
      # Fallback
      text = dplyr$if_else(
        text == "" | is.na(text),
        dplyr$coalesce(latest_developments, overview),
        text
      ),
      # Always add manual_info if available
      text = paste(text, manual_info, sep = " ")
    )

  # now join together and get summarizations
  df_summary <- df_text |>
    dplyr$group_by(iso3, location, date) |>
    dplyr$filter(
      !is.na(text)
    ) |>
    dplyr$mutate(
      summary_long = purrr$map2_chr(
        .x = prompts$long,
        .y = text,
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
      summary_source = "ACAPS reporting"
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

get_inform_justification <- function(iso3_param, date_param, country_param) {
  indicators <- c("People exposed",
                  "People affected",
                  "People displaced",
                  "People in Need",
                  "Crisis affected groups")

  last_day_month <- as.character(format(lubridate$ceiling_date(date_param, "month") - lubridate$days(1), "%Y-%m-%d"))
  httr2$request("https://api.acaps.org/api/v1/inform-severity-index/log") |>
    httr2$req_headers(
      Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
    ) |>
    httr2$req_url_query(
      country = as.character(country_param),
      iso3 = as.character(iso3_param),
      `_internal_filter_date_lte` = last_day_month,
      `_internal_filter_date_gte` = as.character(format(date_param - 365, "%Y-%m-%d")),
      indicator = indicators,
      .multi = "explode"
    ) |>
    httr2$req_retry(
      max_tries = 5,
      backoff = \(x) x / 10
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck(
      "results"
    ) |>
    purrr$map(
      .f = \(x) {
        dplyr$tibble(
          iso3 = unlist(x$iso3),
          country = unlist(x$country),
          regions = unlist(x$regions),
          crisis_id = x$crisis_id,
          crisis_name = x$crisis,
          indicator = x$indicator,
          figure = x$figure,
          justification = x$justification,
          internal_date = as.Date(x$`_internal_filter_date`),
          date =  date_param
        )
      }
    ) |>
    purrr$list_rbind()
}

get_country_info <- function(iso3_param, date_param) {
  httr2$request("https://api.acaps.org/api/v1/countries/") |>
    httr2$req_headers(
      Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
    ) |>
    httr2$req_url_query(
      iso3 = as.character(iso3_param),
      .multi = "explode"
    ) |>
    httr2$req_retry(
      max_tries = 5,
      backoff = \(x) x / 10
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck(
      "results"
    ) |>
    purrr$map(
      .f = \(x) {
        dplyr$tibble(
          iso3 = unlist(x$iso3),
          country = unlist(x$country),
          regions = unlist(x$regions),
          crises = unlist(x$crises),
          overview = x$overview,
          date_request = date_param
        )
      }
    ) |>
    purrr$list_rbind()
}


get_daily_monitoring <- function(date_param, iso3_param) {
  last_day_month <- as.character(format(lubridate$ceiling_date(date_param, "month") - lubridate$days(1), "%Y-%m-%d"))
  df_daily_monitoring <- httr2$request("https://api.acaps.org/api/v1/daily-monitoring/") |>
    httr2$req_headers(
      Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
    ) |>
    httr2$req_url_query(
      iso3 = as.character(iso3_param),
      `_internal_filter_date_lte` = last_day_month,
      `_internal_filter_date_gte` = as.character(format(date_param - 365, "%Y-%m-%d")),
      .multi = "explode"
    ) |>
    httr2$req_retry(
      max_tries = 5,
      backoff = \(x) x / 10
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck(
      "results"
    ) |>
    purrr$map(
      .f = \(x) {
        dplyr$tibble(
          iso3 = unlist(x$iso3),
          country = unlist(x$country),
          latest_developments = x$latest_developments,
          internal_date = as.Date(x$`_internal_filter_date`),
          date =  date_param
        )
      }
    ) |>
    purrr$list_rbind()
}
