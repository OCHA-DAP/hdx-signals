box::use(
  dplyr,
  readr,
  tidyr,
  stringr,
  rvest,
  httr,
  logger
)

#' Creates food insecurity alerts dataset
#'
#' Alerts are generated whenever the current estimate is higher than the previous
#' current estimate, or either projection is higher than current in the analysis.
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
#'
#' @export
alert <- function(df_wrangled) {
  browser()

  # get general alerts
  df_alerts <- df_wrangled |>
    dplyr$mutate(
      any_p5 = phase == "phase5" & `percentage-current` > 0 |
        phase == "phase5" & `percentage-projected` > 0 |
        phase == "phase5" & `percentage-second_projected` > 0
    ) |>
    dplyr$filter(
      phase == "p3plus" & `percentage-current` >= 0.2 |
        phase == "p3plus" & `percentage-projected` >= 0.2 |
        phase == "p3plus" & `percentage-second_projected` >= 0.2 |
        phase == "p4plus" & `percentage-current` >= 0.05 |
        phase == "p4plus" & `percentage-projected` >= 0.05 |
        phase == "p4plus" & `percentage-second_projected` >= 0.05 |
        any_p5
    ) |>
    dplyr$filter(
      `percentage-current` > `percentage-current_lag` & compare_current |
        `percentage-current` < `percentage-projected` |
        `percentage-current` < `percentage-second_projected` |
        any_p5
    ) |>
    dplyr$select(
      -dplyr$starts_with("plot_date")
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
      type = ifelse(stringr$str_detect(name, "projected"), "projected", "current"),
      map_date = ifelse(
        type == "projected" & !is.na(`map_date-projected`),
        `map_date-projected`,
        `map_date-current`
      ),
      phase_level = ifelse(phase_level == 5, "5", paste0(phase_level, "+")),
      analysis_id,
      analysis_area = `analysis_area-current`,
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
      pub_date_diff == min(pub_date_diff, as.difftime(Inf, units = "days")),
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
  url <- "https://www.ipcinfo.org/cadre-harmonise"

  user_agent <- Sys.getenv("IPC_USER_AGENT")
  debug_print <- stringr$str_sub(user_agent, start = 1, end = 2)
  logger$log_info("ch scraper,Start of user agent code: ", debug_print)

  session <- rvest$session(url, httr$user_agent(Sys.getenv("IPC_USER_AGENT")))

  # Read the page
  ch_list <- rvest$read_html(session) |>
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
