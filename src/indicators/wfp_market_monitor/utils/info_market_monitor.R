box::use(dplyr)
box::use(glue)

#' Add campaign info to market monitor alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  df_alerts |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/global-market-monitor",
      source_url = "https://www.wfp.org/publications/market-monitor",
      other_urls = NA_character_,
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a> ',
          "and see the ",
          '<a href="{source_url}">WFP Global Market Monitor</a> to access source ',
          "reports and visit the live explorer."
        )
      )
    )
}
