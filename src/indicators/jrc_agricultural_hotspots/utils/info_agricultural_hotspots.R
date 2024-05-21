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
  df_alerts |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/asap-hotspots-monthly",
      source_url = "https://agricultural-production-hotspots.ec.europa.eu",
      other_urls = NA_character_,
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a>. Visit ',
          'the <a href="{source_url}">ASAP homepage</a> to access additional ',
          "data and use the ASAP Warning Explorer to contextualize the situation."
        )
      )
    )
}
