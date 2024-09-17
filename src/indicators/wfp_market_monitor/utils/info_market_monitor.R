box::use(
  dplyr,
  glue
)

#' Add campaign info to market monitor alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  df_alerts |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/global-market-monitor",
      source_url = paste0("https://dataviz.vam.wfp.org/CountryPage_overview?iso3=", tolower(iso3)),
      other_urls = "https://dataviz.vam.wfp.org/reports?current_page=1&theme=8",
      further_information = as.character(
        glue$glue(
          'Access the data directly <a href="{hdx_url}">on HDX</a> ',
          'and visit the <a href="{source_url}"> {location} overview</a> to find ',
          "key price data and other insights and trends. Current and past editions ",
          "of the Global Market Monitor are available on the ",
          '<a href="{other_urls}">Reports Explorer</a>.'
        )
      )
    )
}
