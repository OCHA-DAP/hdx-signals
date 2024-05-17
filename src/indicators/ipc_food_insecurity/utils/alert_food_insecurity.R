box::use(dplyr)
box::use(readr)
box::use(tidyr)
box::use(stringr)
box::use(ripc)
box::use(rvest)

#' Creates food insecurity alerts dataset
#'
#' Alerts are generated whenever the current estimate is higher than the previous
#' current estimate, or either projection is higher than current in the analysis.
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
alert <- function(df_wrangled) {
  # get general alerts
  df_alerts <- df_wrangled |>
    dplyr$filter(
      phase %in% c("p3plus", "p4plus", "phase5"),
      `percentage-current` > `percentage-current_lag` |
        `percentage-current` < `percentage-projected` |
        `percentage-current` < `percentage-second_projected` |
        phase == "phase5" & `percentage-current` > 0 |
        phase == "phase5" & `percentage-projected` > 0 |
        phase == "phase5" & `percentage-second_projected` > 0
    ) |>
    dplyr$select(
      -starts_with("plot_date")
    ) |>
    dplyr$mutate(
      phase_level = readr$parse_number(phase)
    ) |>
    dplyr$group_by(
      iso3, date
    ) |>
    dplyr$filter(
      phase_level == max(phase_level, -Inf)
    ) |>
    tidyr$pivot_longer(
      cols = dplyr$starts_with("percentage-")
    ) |>
    dplyr$filter(
      value == max(value, -Inf, na.rm = TRUE)
    ) |>
    dplyr$slice(1) |>
    dplyr$ungroup() |>
    dplyr$transmute(
      iso3,
      indicator_name = "food_insecurity",
      indicator_source = "ipc",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      date,
      alert_level_numeric = as.integer(pmin(phase_level - 2, 2)),
      value,
      type = ifelse(stringr$str_detect(name, "projected"), "projected", "estimated"),
      map_date = ifelse(
        type == "projected" & !is.na(`map_date-projected`),
        `map_date-projected`,
        `map_date-current`
      ),
      phase_level = ifelse(phase_level == 5, "5", paste0(phase_level, "+")),
      analysis_id,
      title,
      title_suffix,
      analysis_area,
      link
    )

  # now bring in the URLs closest matching
  ch_df <- ch_scraper()

  # join back to alerts data frame to get CH links
  dplyr$cross_join(
    df_alerts,
    ch_df
  ) |>
    dplyr$group_by(iso3, date) |>
    dplyr$mutate(pub_date_diff = abs(pub_date - date)) |> # find nearest pubs to alert date
    dplyr$filter(
      pub_date_diff == min(pub_date_diff),
      !stringr$str_detect(ch_url, "Tchad") | analysis_id == 68596035, # specific displaced pop pub for Chad
      analysis_id != 68596035 | stringr$str_detect(ch_url, "Tchad")
    ) |>
    dplyr$ungroup() |>
    dplyr$mutate(
      ch = stringr$str_detect(link, "cadre-harmonise"),
      link = ifelse( # bring the specific CH links into the link variable
        ch,
        ch_url,
        link
      )
    )
}

#' Scrapes the CH landing page for publications
#'
#' Scrapes the CH page for publications and returns them plus dates for easy
#' joining with alerts data. This way we have a set of unique URLs for the CH
#' data.
ch_scraper <- function() {
  # extract list of publications from the CH landing page
  ch_list <- rvest$read_html("https://www.ipcinfo.org/cadre-harmonise") |>
    rvest$html_elements(".list-details2")

  # get dates of publication, only look at first dates to match up publications
  # with alerts to nearest start date
  ch_dates <- ch_list |>
    rvest$html_nodes(".list-date") |>
    rvest$html_text() |>
    stringr$str_trunc(width = 10, ellipsis = "") |>
    as.Date(
      format = "%d.%m.%Y"
    )

  # get links
  ch_url <- ch_list |>
    rvest$html_nodes("a") |>
    rvest$html_attr("href")

  dplyr$tibble(
    pub_date = ch_dates,
    ch_url = ifelse(
      stringr$str_detect(ch_url, "http"),
      ch_url,
      paste0("https://www.ipcinfo.org", ch_url)
    )
  )
}
