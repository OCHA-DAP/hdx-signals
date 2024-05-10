box::use(dplyr)
box::use(glue)
box::use(lubridate)

box::use(cs = ../../../../src/utils/cloud_storage)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  # get links to point towards the IDMC website
  df_country_links <- cs$read_az_file("input/idmc_country_links.parquet")

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
      displacement_start_date <= date | (Sys.Date() - displacement_start_date <= 90 & Sys.Date() - date <= 90) # keep recent reports for monitoring
    ) |>
    dplyr$group_by(iso3, date, sources) |>
    dplyr$mutate(
      # number repeated sources and put them into HTML as a list
      sources_num = ifelse(
        n() == 1,
        "",
        paste("#", dplyr$row_number())
      ),
      other_urls_html = paste0(
        '<li><a href="',
        event_url,
        '">',
        sources_num,
        '</a></li>'
      )
    ) |>
    dplyr$summarize( # only keep the first 3 unique URLs
      other_urls = paste(unique(event_url)[1:min(3, length(unique(event_url)))], collapse = "; "),
      other_urls_html = paste0(
        "<ul>\n",
        paste(unique(other_urls_html)[1:min(3, length(unique(event_url)))], collapse = "\n"),
        "</ul>"
      ),
      .groups = "drop"
    ) |>
    dplyr$left_join(
      df_country_links,
      by = "iso3"
    ) |>
    dplyr$mutate(
      hdx_url = as.character(glue$glue("https://data.humdata.org/dataset/idmc-event-data-for-{iso3}")),
      source_url = as.character(glue$glue("https://www.internal-displacement.org/countries/{country_link}")),
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a>, and see the ',
          '<a href="{source_url}">IDMC country page</a> for more information. ',
          'Full context available in the original reports sourced by the IDMC:',
          '\n\n{other_urls_html}'
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
