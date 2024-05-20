box::use(dplyr)
box::use(stringr)
box::use(purrr)
box::use(glue)

box::use(../../../../src/utils/ai_summarizer)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  prompt <- paste(
    "I am passing you text content that contains URL hyperlinks. These hyperlinks",
    "are formatted in varous ways, sometimes incorrectly, and I want you to help me",
    "extract the URL. For a URL {url} and the name of the link for",
    "the reader {url_name}, I want it placed in working HTML a tags",
    'like so: <a href="{url}">{url_name}</a>.\n',
    "URls should be placed in there own <a> tags, and all put into an",
    "HTML ordered list, so <ol><li><a>...</a></li><li><a>...</a></li></ol>.",
    "The input text may have existing HTML <a> tags that are incorrect. For instance",
    'some look like: <a href="{url}]{url_name}</a>, where the " is not closed and',
    "] used instead of >, so the HTML is invalid. Sometimes the closing tag is",
    'missing: `<a href="{url}]{url_name}. Other instances just have URLs in between',
    "parentheses () with no {url_name}, or multiple URLs are repeated one after another,",
    "so they should all be placed within individual <a> tags.\n",
    "Sometimes, there is no clear {url_name} that should appear between the HTML",
    "tags. In this case, please create a very simple {url_name} to go between the",
    "tags. These should be short and simple, like 'May MARS bulletin' or 'FAO Country Brief'\n",
    "Please only return the ordered list of URLs. Here is the text to extract them from --> "
  )

  # join up to get comment
  df_alerts <- dplyr$left_join(
    df_alerts,
    df_wrangled |> dplyr$select(iso3, date, comment),
    by = c("iso3", "date")
  )

  # check what content text has URLs and extract them
  has_url <- stringr$str_detect(df_alerts$comment, "http|href|www")
  df_alerts$url_list <- NA_character_
  df_alerts$url_list[has_url] <- purrr$map_chr(
    .x = df_alerts$comment[has_url],
    .f = \(info) ai_summarizer$ai_summarizer(prompt = prompt, info = info)
  )

  # get URL information
  df_alerts$url_information <- ifelse(
    has_url,
    paste0(
      "<br><br>Source reports for this hotspot are:<br>",
      df_alerts$url_list
    ),
    ""
  )

  # get other URLs by only checking rows with URLs
  df_alerts$other_urls <- NA_character_
  df_alerts$other_urls[has_url] <- stringr$str_match_all(
    string = df_alerts$url_information[has_url],
    pattern = '"(.*?)"'
  ) |>
    purrr$map_chr(
      .f = \(x) paste(x[, 2], collapse = "; ")
    )

  df_alerts |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/asap-hotspots-monthly",
      source_url = "https://agricultural-production-hotspots.ec.europa.eu",
      other_urls,
      further_information = glue$glue(
        'Access the data directly <a href="{hdx_url}">on HDX</a>. Visit ',
        'the <a href="{source_url}">ASAP homepage</a> to access additional ',
        "data and use the ASAP Warning Explorer to contextualize the situation.",
        url_information
      )
    )
}
