box::use(
  dplyr,
  glue,
  lubridate
)

box::use(cs = src/utils/cloud_storage)

#' Add campaign info to IDMC displacement alerts
#'
#' Builds the campaign links shown to Signals subscribers for an IDMC
#' displacement alert: the HDX dataset the signal was detected in, the IDMC
#' country page, and up to three of the most recent source reports behind the
#' displacement figures.
#'
#' @param df_alerts Data frame of alerts, one row per `iso3` and `date`.
#' @param df_wrangled Data frame of wrangled displacement data. Unused, kept for
#'   consistency with the `info()` signature shared by all indicators.
#' @param df_raw Data frame of raw IDMC IDU events, providing the
#'   `event_url`/`sources` pairs used as source reports.
#'
#' @returns Data frame with campaign information, one row per row of
#'   `df_alerts`, with columns `hdx_url`, `source_url`, `other_urls` and
#'   `further_information`.
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  # get links to point towards the IDMC website
  df_links <- cs$read_az_file("input/idmc_urls.csv")

  # get links for all the source urls to provide to the user
  df_source_links <- df_raw |>
    dplyr$select(
      iso3,
      displacement_start_date,
      displacement_end_date,
      event_url,
      sources
    )

  # now join together and get key information
  df_info <- df_alerts |>
    dplyr$full_join(
      df_source_links,
      by = "iso3"
    ) |>
    dplyr$group_by(iso3, date) |>
    dplyr$filter(
      displacement_end_date >= date - lubridate$days(30),
      # keep recent reports for monitoring
      displacement_start_date <= date | (Sys.Date() - displacement_start_date <= 90 & Sys.Date() - date <= 90)
    ) |>
    dplyr$arrange(
      dplyr$desc(displacement_start_date)
    ) |>
    dplyr$distinct(
      event_url, sources
    ) |>
    dplyr$mutate(
      total_n = dplyr$n()
    ) |>
    dplyr$slice_head(
      n = 3
    ) |>
    dplyr$mutate(
      other_urls_html = paste0(
        '<li><a href="',
        event_url,
        '">',
        sources,
        "</a></li>"
      )
    ) |>
    dplyr$summarize( # only keep the first 3 unique URLs
      other_urls = paste(unique(event_url)[seq_len(min(3, length(unique(event_url))))], collapse = "; "),
      other_urls_html = paste0(
        "<ol>\n",
        paste(unique(other_urls_html)[seq_len(min(3, length(unique(event_url))))], collapse = "\n"),
        "</ol>"
      ),
      total_n = unique(total_n),
      .groups = "drop"
    ) |>
    dplyr$left_join(
      df_links,
      by = "iso3"
    ) |>
    dplyr$mutate(
      # IDMC IDU datasets are published on HDX under the slug
      # "{iso3}-idmc-idu-events", lowercased (e.g. mmr-idmc-idu-events)
      hdx_url = as.character(
        glue$glue("https://data.humdata.org/dataset/{tolower(iso3)}-idmc-idu-events")
      ),
      source_url = url,
      source_url_info = ifelse(
        is.na(url),
        ".",
        glue$glue(
          ", and see the ",
          '<a href="{source_url}">IDMC page</a> for more information.'
        )
      ),
      n_text = dplyr$case_when(
        total_n == 1 ~ "report",
        total_n == 2 ~ "two reports",
        total_n == 3 ~ "three reports",
        total_n > 3 ~ "three most recent reports"
      ),
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a>{source_url_info} ',
          "Get context from the {n_text} sourced by the IDMC:",
          "\n\n{other_urls_html}"
        )
      )
    )

  # ensuring the output matches the original input
  df_alerts |>
    dplyr$left_join(
      df_info,
      by = c("iso3", "date")
    ) |>
    dplyr$select(
      hdx_url,
      source_url,
      other_urls,
      further_information
    )
}
