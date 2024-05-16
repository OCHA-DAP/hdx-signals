box::use(dplyr)
box::use(glue)
box::use(lubridate)

box::use(cs = ../../../../src/utils/cloud_storage)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  df_alerts |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/global-acute-food-insecurity-country-data",
      source_url = link,
      source_url_text = ifelse(
        source_url == "http://www.ipcinfo.org/cadre-harmonise",
        '<a href="{source_url}">CH page</a> to download the latest CH report',
        '<a href="{source_url}">IPC webmap</a> for more information'
      ),
      other_urls = NA_character_,
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a>, and see the {source_url_text}.'
        )
      )
    ) |>
    dplyr$select(
      -source_url_text
    )
}
