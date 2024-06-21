box::use(dplyr)
box::use(purrr)
box::use(stringr)
box::use(glue)
box::use(rvest)

box::use(src/utils/ai_summarizer)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  df_urls_info <- df_alerts$comment |>
    purrr$map(
      .f = scrape_urls
    ) |>
    purrr$list_rbind()

  df_alerts |>
    dplyr$bind_cols(df_urls_info) |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/asap-hotspots-monthly",
      source_url = "https://agricultural-production-hotspots.ec.europa.eu",
      other_urls,
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a>. Visit ',
          'the <a href="{source_url}">ASAP homepage</a> to access additional ',
          "data and use the ASAP Warning Explorer to contextualize the situation.",
          "{other_urls_info}"
        )
      )
    )
}

#' Scrape URLs and names from JRC ASAP content
#'
#' @param x Comment string
#'
#' @returns Data frame of URLs to be used as `source_url`
scrape_urls <- function(x) {
  a_nodes <- x |>
    rvest$minimal_html() |>
    rvest$html_elements(css = "a")
  # get nodes with valid urls
  urls <- rvest$html_attr(a_nodes, "href")
  no_urls <- is.na(urls) | !stringr$str_detect(urls, "http|www")

  if (all(no_urls)) {
    return(
      dplyr$tibble(
        other_urls = NA_character_,
        other_urls_info = ""
      )
    )
  }

  # paste these together into a list
  dplyr$tibble(
    other_urls = paste(urls[!no_urls], collapse = "; "),
    other_urls_info = paste0(
      "<p><ol>",
      paste("<li>", a_nodes[!no_urls], "</li>", collapse = ""),
      "</ol></p>"
    )
  )
}
