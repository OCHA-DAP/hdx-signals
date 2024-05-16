box::use(dplyr)
box::use(purrr)
box::use(ripc)
box::use(rvest)
box::use(stringr)

box::use(../../../../src/utils/ai_summarizer)

#' Generate summary for food insecurity alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  # get the links based on the analysis ID
  df_links <- ripc$ipc_get_analyses() |>
    dplyr$select(
      analysis_id,
      link
    )

  df_alerts |>
    dplyr$left_join(
      df_links,
      by = "analysis_id"
    ) |>
    dplyr$mutate(
      summary_long = purrr$map_chr(
        .x = link,
        .f = ipc_scraper
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
      )
    ) |>
    dplyr$select(
      summary_long,
      summary_short
    )
}

#' Scrapes IPC URL for information
#'
#' Scrapes the IPC URL for information and passes this on to the API.
#' Currently only works for URL links that are not cadre-harmonise, which
#' are the links directly to source reports.
#'
#' @param url URL from the IPC dataset
ipc_scraper <- function(url) {
  if (!is.na(url) & url != "http://www.ipcinfo.org/cadre-harmonise") {
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
  } else {
    NA_character_
  }
}
