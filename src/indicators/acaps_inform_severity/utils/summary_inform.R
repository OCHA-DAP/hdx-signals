box::use(
  httr2,
  dplyr,
  tidyr,
  purrr
)
box::use(
  src/utils/ai_summarizer,
  src/utils/get_prompts,
  src/utils/get_env
)

#' Add campaign info to ACAPS inform data
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts("acaps_inform_severity")

  # get all the event info from the ACAPS api
  # Get justifications
  indicators <- c("People exposed",
                  "People affected",
                  "People displaced",
                  "People in Need",
                  "Crisis affected groups")

  #Call api for each alert to get Justification
  df_wrangled$date <- as.Date(df_wrangled$date)
  df_alerts$date <- as.Date(df_alerts$date)
  df_wrangled_alerting <- dplyr$inner_join(df_wrangled, df_alerts, by = c("iso3", "date"))

  df_info <- dplyr$tibble(
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

  df_risk <- dplyr$tibble(
    iso3 = character(),
    risk_title = character(),
    rationale = character(),
    vulnerability = character(),
    internal_date = as.Date(character()),
    date = as.Date(character())
  )
  for (ii in seq_len(nrow(df_wrangled_alerting))){
    df_event_info <- httr2$request("https://api.acaps.org/api/v1/inform-severity-index/log") |>
      httr2$req_headers(
        Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
      ) |>
      httr2$req_url_query(
        crisis_id = as.character(df_wrangled_alerting[ii, "crisis_id"]),
        iso3 = as.character(df_wrangled_alerting[ii, "iso3"]),
        `_internal_filter_date_lte` = as.character(format(df_wrangled_alerting[ii, "date"] - 0, "%Y-%m-%d")),
        `_internal_filter_date_gte` = as.character(format(df_wrangled_alerting[ii, "date"] - 365, "%Y-%m-%d")),
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
            date =  dplyr$pull(df_wrangled_alerting[ii, "date"])
          )
        }
      ) |>
      purrr$list_rbind()
    df_info <- dplyr$bind_rows(list(df_info, df_event_info))
    df_event_risk <- httr2$request("https://api.acaps.org/api/v1/risk-list/") |>
      httr2$req_headers(
        Authorization = paste("Token", get_env$get_env("ACAPS_TOKEN"))
      ) |>
      httr2$req_url_query(
        iso3 = as.character(df_wrangled_alerting[ii, "iso3"]),
        crisis_id = as.character(df_wrangled_alerting[ii, "crisis_id"]),
        `_internal_filter_date_gte` = as.character(format(df_wrangled_alerting[ii, "date"] - 365, "%Y-%m-%d")),
        `_internal_filter_date_lte` = as.character(dplyr$pull(df_wrangled_alerting[ii, "date"]) + months(1)),
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
          # Debug lengths
          lengths <- sapply(x, function(col) if (is.null(col)) 0 else length(unlist(col)))

          # Find maximum length to align columns
          max_length <- max(lengths)

          dplyr$tibble(
            iso3 = rep(unlist(x$iso3), length.out = max_length),
            risk_title = rep(unlist(x$risk_title), length.out = max_length),
            rationale = rep(unlist(x$rationale), length.out = max_length),
            vulnerability = rep(unlist(x$vulnerability), length.out = max_length),
            crisis_id = rep(unlist(x$crisis_id), length.out = max_length),
            internal_date = rep(as.Date(x$`_internal_filter_date`), length.out = max_length),
            date = rep(dplyr$pull(df_wrangled_alerting[ii, "date"]), length.out = max_length)
          )
        }
      )
    df_event_risk <- if (length(df_event_risk) != 0) {
      purrr$list_rbind(df_event_risk) |> dplyr$filter(iso3 == as.character(df_wrangled_alerting[ii, "iso3"]))
    } else {
      df_risk[0, ]
    }
    df_risk <- dplyr$bind_rows(list(df_risk, df_event_risk))
  }

  df_risk <- df_risk |>
    dplyr$group_by(iso3, crisis_id) |>
    dplyr$filter(internal_date == max(internal_date))

  df_risk <- df_risk |>
    dplyr$group_by(iso3, crisis_id, date) |>
    dplyr$summarize(text2 = paste(risk_title, rationale, vulnerability, sep = ": ", collapse = ", "), .groups = "drop")

  df_info <- df_info |>
    dplyr$group_by(iso3, crisis_id, indicator) |>
    dplyr$filter(internal_date == max(internal_date))
  id_cols <- c("iso3", "country", "crisis_id", "crisis_name", "date")

  df_info <- df_info |>
    dplyr$mutate(text = paste(indicator, figure, justification, sep = ": ")) |>
    tidyr$pivot_wider(id_cols = id_cols, names_from = indicator, values_from = text) |>
    dplyr$ungroup() |>
    dplyr$mutate(text = do.call(
      paste,
      c(dplyr$across(-dplyr$all_of(id_cols)), sep = " ")
    )) |>
    dplyr$select(c("iso3", "date", "crisis_id", "text"))

  df_info <- df_info |>
    dplyr$full_join(df_risk, by = c("iso3", "crisis_id", "date")) |>
    dplyr$select(c("iso3", "date", "text", "text2"))

  # now join together and get summarizations
  df_summary <- df_alerts |>
    dplyr$full_join(
      df_info,
      by = c("iso3", "date")
    ) |>
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
    ) |>
    dplyr$mutate(
      summary_long1 = purrr$map2_chr(
        .x = prompts$long,
        .y = text,
        .f = ai_summarizer$ai_summarizer
      ),
      summary_long2 = purrr$map2_chr(
        .x = prompts$long,
        .y = text2,
        .f = ai_summarizer$ai_summarizer
      ),
      summary_short_temp = ifelse(
        is.na(summary_long1) | summary_long1 == "" | is.na(summary_long2) | summary_long2 == "",
        plot_title,
        purrr$pmap_chr(
          .l = list(
            prompt = prompts$short,
            info = paste(summary_long1, summary_long2, sep = ","),
            location = location
          ),
          .f = ai_summarizer$ai_summarizer_without_location
        )
      ),
      summary_short = ifelse(
        is.na(summary_short_temp) | summary_short_temp == "",
        plot_title,
        summary_short_temp
      ),
      summary_long = ifelse(
        is.na(summary_long1) | summary_long1 == "",
        summary_long2,
        summary_long1
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
