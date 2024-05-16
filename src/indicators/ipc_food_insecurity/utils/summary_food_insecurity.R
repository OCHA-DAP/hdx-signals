box::use(dplyr)
box::use(purrr)
box::use(rvest)
box::use(stringr)
box::use(pdftools)

box::use(../../../../src/utils/ai_summarizer)

#' Generate summary for food insecurity alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  df_alerts |>
    dplyr$mutate(
      summary_long = purrr$pmap_chr(
        .l = list(
          url = link,
          country = country,
          date = date
        ),
        .f = ipc_ch_summarizer
      ),
      prompt_short = paste(
        "Please condense the following information into a single 10 word line,",
        "similar to text you might see on a news ticker. Outputs could look like",
        "the following 2 examples:",
        "'Food insecurity in the capital worsens following failed rainy season' or",
        "'Seasonal forecasts indicate deteriorating food security in the northwest'.",
        "Expect the reader to have no context, but this is",
        "intended to capture their attention, so keep the messaging simple, clear",
        "and punchy. Use only the information below in your summary:\n\n"
      ),
      summary_short = purrr$map2_chr(
        .x = prompt_short,
        .y = summary_long,
        .f = \(prompt, info) {
          if (is.na(info)) {
            return(NA_character_)
          }
          ai_summarizer$ai_summarizer(prompt = prompt, info = info)
        }
      ),
      summary_short = ifelse(
        phase_level == "5",
        paste0(
          "<b><i>Phase 5 alert<b></i> - ",
          summary_short
        ),
        summary_short
      )
    ) |>
    dplyr$select(
      summary_long,
      summary_short
    )
}

#' Summarizes IPC/CH data
#'
#' Based on the URL from the dataset provided, summarizes data using Ai on scraped
#' web pages and documents. If an IPC dataset, scrapes the webmap and situation
#' summary from the URL provided. If a CH dataset, where no country specific page
#' is available, then CH PDFs are scraped to find the most relevant one and country
#' information extracted.
#'
#' @param url URL for the analysis
#' @param date Date of the campaign
#' @param country Country name
#'
#' @returns Summarized text data
ipc_ch_summarizer <- function(url, date, country) {
  if (is.na(url)) {
    return(NA_character_)
  } else if (url == "http://www.ipcinfo.org/cadre-harmonise") {
    ch_scraper(country = country, date = date)
  } else {
    ipc_scraper(url)
  }
}

#' Scrapes IPC URL for information
#'
#' Scrapes the IPC URL for information and passes this on to the API.
#' Currently only works for URL links that are not cadre-harmonise, which
#' are the links directly to source reports.
#'
#' @param url URL from the IPC dataset
ipc_scraper <- function(url) {
  txt <- rvest$read_html(url) |>
    rvest$html_nodes("._undmaptext") |>
    rvest$html_text()
  # extract the situation report and recommendations from the scraped text
  txt <- txt[rank(-nchar(txt)) <= 2]
  txt <- stringr$str_replace_all(txt, "[\r\n\t]+", " ")

  # feed these to the AI to get a summarization
  sit_rep <- ai_summarizer$ai_summarizer(
    prompt = paste(
      "Please shortly summarize the current food insecurity situation in 2 to 3",
      "sentences. Expect the reader to be familiar with the terminology and general",
      "context, but wants to know exactly what is happening",
      "based on information provide in the following description -->"
    ),
    info = txt[1]
  )
  recs <- ai_summarizer$ai_summarizer(
    prompt =  paste(
      "Please shortly summarize the key recommendations in 2 to 3",
      "sentences. Expect the reader to be familiar with the terminology and general",
      "context, but wants a quick summary of recommendations from",
      "the following description -->"
    ),
    info = txt[2]
  )

  # ensure that we are only using those that are not blank
  # so make sure to check when parts of it are not available
  if (is.na(sit_rep) & is.na(recs)) {
    NA_character_
  } else if (is.na(sit_rep)) {
    paste0(
      "<b>Recommendations:</b><br><br>",
      recs
    )
  } else if (is.na(recs)) {
    paste0(
      "<b>Overview: </b><br><br>",
      sit_rep
    )
  } else {
    paste(
      "<b>Overview:</b><br><br>",
      sit_rep,
      "<br><br><b>Recommendations:</b><br><br>",
      recs
    )
  }

}

#' Scrapes CH publications for information
#'
#' Since there are no country publications with specific links for the CH,
#' we use the `date` of an alert to find the nearest CH publication based on its
#' start date, and then read that in and ingest for context.
#'
#' @param date Date to match publication to
#'
#' @returns Summary of text from the publication
ch_scraper <- function(country, date) {
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

  # filter links to closest date
  idx <- which(min(abs(date - ch_dates)))
  urls_to_scrape <- ch_url[idx]

  # get text from the PDFs for info for AI prompt
  info <- purrr$map_chr(
      .x = url_to_scrape,
      .y = pdftools$pdf_text
    ) |>
    paste(
      sep = "\n",
      collapse = "\n"
    )

  prompt <- paste(
    "I need a summary of the food security situation in",
    country,
    "based on the following document. The document is written in French, and about",
    "multiple countries, but I only want information about",
    country,
    "in the summarization. There are a lot of random numbers in the text because",
    "the text was scraped from a PDF with tables, so please just focus on the",
    "explanatory text, not numeric figures. Assume the reader is familiar with",
    "food insecurity and general",
    "context, but wants to know what is happening based on this report, and also",
    "interested in what actions are recommended. Please produce a text summary",
    "with two sections, Overview:, which provides a general situation summary",
    "and recommendations, which is the recommendation actions, both for",
    country,
    ". The final format should look like:",
    "'<b>Overview</b>: {overview text here}<br><br><b>Recommendations</b>:<br><br> {recommendations text}',",
    "usingHTML <br> tags to add lines to the output and bold tags on the headings.",
    "Here is the information to use for summarization --> "
  )
  ai_summarizer(info = info, prompt = prompt)
}
