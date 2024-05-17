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
          ch = ch
        ),
        .f = ipc_ch_summarizer
      ),
      prompt_short = paste(
        "Please condense the following information into a single 10 word line,",
        "similar to text you might see on a news ticker. Outputs could look like",
        "the following 2 examples:",
        "'Food insecurity in the capital worsens following failed rainy season' or",
        "'Seasonal forecasts indicate deteriorating food security in the northwest'.",
        "Do not include the country name in the output.",
        "This output is invalid: 'Burkina Faso projected to experience worsening",
        "food insecurity due to conflict, instability'.",
        "Expect the reader to have no context, but this is",
        "intended to capture their attention, so keep the messaging simple, clear",
        "and punchy. Use only the information below in your summary:\n\n"
      ),
      summary_short = purrr$pmap_chr(
        .l = list(
          prompt = prompt_short,
          info = summary_long,
          country = country
        ),
        .f = ai_summarizer$ai_summarizer_without_country
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
#' @param ch If `TRUE` it's a CH analysis, otherwise it's IPC
#'
#' @returns Summarized text data
ipc_ch_summarizer <- function(url, ch, country) {
  if (is.na(url)) {
    return(NA_character_)
  } else if (ch) {
    ch_summarizer(url = url, country = country)
  } else {
    txt <- ipc_scraper(url)
    ipc_summarizer(txt)
  }
}

#' Scrapes IPC URL for information
#'
#' Scrapes the IPC URL for information and passes this on to the API.
#' Currently only works for URL links that are not cadre-harmonise, which
#' are the links directly to source reports.
#'
#' @param url URL from the IPC dataset
#'
#' @returns Text from the website, with recommendations and summaries
ipc_scraper <- function(url) {
  txt <- rvest$read_html(url) |>
    rvest$html_nodes("._undmaptext") |>
    rvest$html_text()
  # extract the situation report and recommendations from the scraped text
  txt <- txt[rank(-nchar(txt)) <= 2]
  txt <- stringr$str_replace_all(txt, "[\r\n\t]+", " ")
  txt
}

#' Summarize IPC text data
#'
#' Scrapes the IPC webmaps for country reports.
#'
#' @param txt Text scraped from IPC website using `ipc_scraper()`
#'
#' @returns AI summarization
ipc_summarizer <- function(txt) {
  # feed these to the AI to get a summarization
  sit_rep <- ai_summarizer$ai_summarizer(
    prompt = paste(
      "Please shortly summarize the current food insecurity situation in 2 to 3",
      "sentences. Expect the reader to be familiar with the terminology and general",
      "context, but wants to know exactly what is happening",
      "based on information provided. Avoid using",
      "too many technical details, and just get the key points across.",
      "Use the following data to generate the output --> "
    ),
    info = txt[1]
  )
  recs <- ai_summarizer$ai_summarizer(
    prompt =  paste(
      "Please shortly summarize the key recommendations in 2 to 3",
      "sentences. Expect the reader to be familiar with the terminology and general",
      "context, but wants a quick summary of recommendations from the data.",
      "Avoid using technical jargon and just get the key points across.",
      "Use the following data to generate the output --> "
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

#' Parses CH publications for information
#'
#' Since there are (typically) no country publications with specific links for the CH,
#' each one is a URL of a PDF. We parse this and pass for summarization.
#'
#' @param url URL to the PDF publications
#'
#' @returns Summary of text from the publication
ch_summarizer <- function(url, country) {
  # get text from the PDFs for info for AI prompt
  info <- pdftools$pdf_text(url) |>
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
    "'<b>Overview</b>:<br><br> {overview text}<br><br><b>Recommendations</b>:<br><br> {recommendations text}',",
    "usingHTML <br> tags to add lines to the output and bold tags on the headings.",
    "Ensure that the outputs {overview text} and {recommendations text} are",
    "limited in length to 3 sentences, 4 at most.",
    "Here is the information to use for summarization --> "
  )
  ai_summarizer$ai_summarizer(info = info, prompt = prompt)
}
