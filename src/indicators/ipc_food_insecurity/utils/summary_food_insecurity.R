box::use(dplyr)
box::use(purrr)
box::use(rvest)
box::use(stringr)
box::use(glue)
box::use(pdftools)

box::use(../../../../src/utils/ai_summarizer)
box::use(../../../../src/utils/get_prompts)
box::use(../../../../src/utils/parse_pdf)

#' Generate summary for food insecurity alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  prompts <- get_prompts$get_prompts(
    indicator_id = "ipc_food_insecurity",
    prompts = "short"
  )

  df_alerts |>
    dplyr$mutate(
      summary_long = purrr$pmap_chr(
        .l = list(
          url = link,
          ch = ch,
          location = location
        ),
        .f = ipc_ch_summarizer
      ),
      summary_short = ifelse(
        is.na(summary_long),
        plot_title, # use the plot title if no text to summarize
        purrr$pmap_chr(
          .l = list(
            prompt = prompts$short,
            info = summary_long,
            location = location
          ),
          .f = ai_summarizer$ai_summarizer_without_location
        )
      ),
      summary_short = ifelse(
        phase_level == "5",
        paste0(
          "<b><i>Phase 5 alert<b></i> - ",
          summary_short
        ),
        summary_short
      ),
      summary_source = dplyr$case_when(
        is.na(summary_long) ~ NA_character_,
        ch ~ "CH reports",
        .default = "IPC analyses"
      )
    ) |>
    dplyr$select(
      summary_long,
      summary_short,
      summary_source,
    )
}

#' Summarizes IPC/CH data
#'
#' Based on the URL from the dataset provided, summarizes data using Ai on scraped
#' web pages and documents. If an IPC dataset, scrapes the webmap and situation
#' summary from the URL provided. If a CH dataset, where no location specific page
#' is available, then CH PDFs are scraped to find the most relevant one and location
#' information extracted.
#'
#' @param url URL for the analysis
#' @param ch If `TRUE` it's a CH analysis, otherwise it's IPC
#' @param location Name of the location
#'
#' @returns Summarized text data
ipc_ch_summarizer <- function(url, ch, location) {
  if (is.na(url)) {
    return(NA_character_)
  } else if (ch) {
    txt <- ch_scraper(url = url)
    org <- "ch"
  } else {
    txt <- ipc_scraper(url)
    # check that scraping was successful and exit early if not
    if (length(txt) == 0 || all(is.na(txt)) || (length(txt) == 1 && nchar(txt) < 100)) {
      return(NA_character_)
    }
    org <- "ipc"
  }

  text_summarizer(txt = txt, org = org)
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
#' Scrapes the IPC webmaps for location reports.
#'
#' @param txt Text scraped from IPC website using `ipc_scraper()` or sourced
#'     from CH documents. The first value is used for recommendation summaries,
#'     and the second value for situation summaries, because that is how the parsing
#'     is returned from the IPC. It is just the same document for CH.
#'
#' @returns AI summarization
text_summarizer <- function(txt, org) {
  prompts <- get_prompts$get_prompts(
    indicator_id = "ipc_food_insecurity",
    prompts = paste0(org, c("_sit_rep", "_recs"))
  )

  # feed these to the AI to get a summarization
  recs <- ai_summarizer$ai_summarizer(
    prompt = glue$glue(prompts[[1]]),
    info = txt[1]
  )

  sit_rep <- ai_summarizer$ai_summarizer(
    prompt = glue$glue(prompts[[2]]),
    info = txt[2]
  )

  # ensure that we are only using those that are not blank
  # so make sure to check when parts of it are not available
  if (is.na(sit_rep) && is.na(recs)) {
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
#' Since there are (typically) no location publications with specific links for the CH,
#' each one is a URL of a PDF. We parse this very simply so it can be passed
#' for summarisation.
#'
#' @param url URL to the PDF publications
#'
#' @returns Summary of text from the publication
ch_scraper <- function(url) {
  # get text from the PDFs for info for AI prompt
  txt <- parse_pdf$parse_pdf(url)

  # same text needs to be used for recommendations and summarisations
  c(txt, txt)
}
