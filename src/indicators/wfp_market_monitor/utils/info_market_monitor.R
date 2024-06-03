box::use(dplyr)
box::use(glue)

box::use(cs = ../../../../src/utils/cloud_storage)

#' Add campaign info to market monitor alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  df_alerts |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/wfp-market-monitoring",
      source_url = "https://www.wfp.org/publications/market-monitor",
      other_urls = NA_character_,
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a> (dataset pending) ',
          "and see the ",
          '<a href="{source_url}">WFP Global Market Monitor</a> to access source ',
          "reports and visit the live explorer."
        )
      )
    )
}
