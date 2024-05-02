box::use(../utils/ai_summarizer[ai_summarizer])

#' Generate the summary for a campaign
#'
#' Generates the text summary for a campaign from all country information. Used
#' to provide intro text to the campaign email. Information is pulled from
#' `df_campaign_content`, which has country level summarizations. Campaign
#' level summaries uses the `plot_title` information as key figures for each
#' country, and the `summary_long` as the situation summary when available.
#'
#' If `summary_long` is unavailable, then the prompt requests just a single,
#' short introduction. If it is available, then a short summary of 2 to 3 sentences
#' is requested in the prompt.
#'
#' @param df_campaign_content
#'
#' @returns Campaign summary string
#'
#' @export
generate_campaign_summary <- function(df_campaign_content) {
  # to do: remove after testing
  return(NA_character_)
  # limited country information available, so keep the prompt extremely limited
  if (all(is.na(df_campaign_content$summary_long))) {
    if(all(is.na(df_campaign_content$plot_title))) {
      NA_character_
    } else {
      ai_summarizer(
        prompt = paste(
          "I will provide you information from an email highlighting concerning",
          "situations in different countries. Can you please write an introductory",
          "sentence for the whole email? Keep the sentence very short, and just use",
          "the limited information below. This text you generate will be directly",
          "put into the email so do not include any headings or extraneous text.",
          "We want to grab the readers attention, so make it punchy."
        ),
        info = paste(df_campaign_content$plot_title, collapse = ". ")
      )
    }
  } else {
    # generate the full summary if more countries or more info available
    ai_summarizer(
      prompt = paste(
        "I will provide you information from an email highlighting concerning",
        "situations in different countries. For each country, information is",
        "presented in the format {Key figure}: {text summary}', so a short key",
        "figure sentence followed by a longer text summary of the situation,",
        "separated by a colon. Country sections are separated by line breaks.",
        "Use this information to write a short introduction for the email.",
        "Keep the section very short,",
        min(nrow(df_campaign_content) + 1, 3), # request 2 sentences if only 1 country alerting, 3 otherwise
        "sentences at most, and just use the",
        "information provided below. This text you generate will be directly",
        "put into the email so do not include any headings or extraneous text.",
        "We want to grab the readers attention, so make it punchy."
      ),
      info = paste(
        df_campaign_content$plot_title,
        df_campaign_content$summary_long,
        sep = ": ",
        collapse = "\n"
      )
    )
  }
}
