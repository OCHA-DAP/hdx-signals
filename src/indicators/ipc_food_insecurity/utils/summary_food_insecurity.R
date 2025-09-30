box::use(
  dplyr,
  purrr,
  rvest,
  stringr,
  glue,
  httr,
  logger
)

box::use(
  src/utils/ai_summarizer,
  src/utils/get_prompts,
  src/utils/parse_pdf,
  src/utils/cloud_storage,
  src/utils/validate_manual_info,
  scr/utils/read_manual_info,
  src/utils/get_manual_info
)

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
          location = location,
          iso3 = iso3,
          indicator_id = indicator_id,
          date = date
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
#' @param iso3  ISO3 country code (character string)
#' @param indicator_id The Signals indicator identifier (character string)
#' @param date A character string or Date object in YYYY-MM-DD format
#' @returns Summarized text data
ipc_ch_summarizer <- function(url, ch, location, iso3, indicator_id, date) {
  scraper_result <- NULL
  manual_result <- NULL

  # Always try to get manual data
  manual_txt <- get_manual_info(iso3, indicator_id, date)
  if (!is.null(manual_txt)) {
    manual_result <- manual_txt
  }

  # Scrape data if URL is available
  if (!is.na(url)) {
    if (ch) {
      txt <- ch_scraper(url = url)
      if (!is.null(txt) && length(txt) > 0 && !all(is.na(txt))) {
        scraper_result <- txt
        org = "ch"
      }
    } else {
      txt <- ipc_scraper(url)
      # Check that scraping was successful
      if (length(txt) > 0 && !all(is.na(txt)) && !(length(txt) == 1 && nchar(txt) < 100)) {
        scraper_result <- txt
        org = "ipc"
      }
    }
  }

  # Combine results
  if (!is.null(scraper_result) && !is.null(manual_result)) {
    # Both available - combine them
    return(text_summarizer_combined(scraper_result = scraper_result, manual_result = manual_result, org = org))
  } else if (!is.null(scraper_result)) {
    # Scraper only
    return(text_summarizer(txt = txt, org = org))
  } else if (!is.null(manual_result)) {
    # Manual only
    return(text_summarizer_manual(manual_result))
  } else {
    # None available
    return(NA_character_)
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
  user_agent <- Sys.getenv("IPC_USER_AGENT")

  debug_print <- stringr$str_sub(user_agent, start = 1, end = 2)
  logger$log_info("Start of user agent code: ", debug_print)


  session <- rvest$session(url, httr$user_agent(Sys.getenv("IPC_USER_AGENT")))

  txt <- rvest$read_html(session) |>
    rvest$html_nodes("._undmaptext") |>
    rvest$html_text()
  # extract the situation report and recommendations from the scraped text
  txt <- txt[rank(-nchar(txt)) <= 2]
  txt <- stringr$str_replace_all(txt, "[\r\n\t]+", " ")
  txt
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



text_summarizer_manual <- function(txt_c) {
  # Check that input is a list of exactly two strings
  if (!is.list(txt_c) || length(txt_c) != 2) {
    stop("Input must be a list of exactly two strings")
  }

  # Load manual prompts from files
  prompts <- list(
    readLines("manual_sit_rep.txt", warn = FALSE),
    readLines("manual_recs.txt", warn = FALSE)
  )

  # Combine prompt file lines into single strings
  prompts <- lapply(prompts, function(x) paste(x, collapse = "\n"))

  # Process situation and recommendations using AI
  sit_rep <- ai_summarizer$ai_summarizer(
    prompt = glue$glue(prompts[[1]]),
    info = txt_c[[1]]  # First string for situation
  )

  recs <- ai_summarizer$ai_summarizer(
    prompt = glue$glue(prompts[[2]]),
    info = txt_c[[2]]  # Second string for recommendations
  )

  # Handle cases where parts of content are not available
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


text_summarizer_combined <- function(scraper_result, manual_result, org) {
  # Extract individual elements
  scraper_situation <- scraper_result[1]
  scraper_recommendations <- scraper_result[2]

  manual_situation <- manual_result[1]
  manual_recommendations <- manual_result[2]

  # Combine situation (scraper + manual)
  combined_situation <- paste(scraper_situation,
                              "\n",
                              manual_situation)

  # Combine recommendations (scraper + manual)
  combined_recommendations <- paste(scraper_recommendations,
                                    "\n",
                                    manual_recommendations)

  # Return combined vector
  txt <- c(combined_situation, combined_recommendations)

  return(text_summarizer(txt, org))
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
  sit_rep <- ai_summarizer$ai_summarizer(
    prompt = glue$glue(prompts[[1]]),
    info = txt[1]
  )

  recs <- ai_summarizer$ai_summarizer(
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

