box::use(
  dplyr,
  glue
)

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
        ch,
        '<a href="{source_url}">CH report</a> for more information. Note that the report is in French',
        '<a href="{source_url}">IPC webmap</a> for more information'
      ),
      analysis_area_text = dplyr$case_when(
        is.na(analysis_area) ~ NA_character_,
        analysis_area == "" ~ ".",
        TRUE ~ glue$glue(": {analysis_area}.")
      ),
      partial_coverage_text = ifelse(
        is.na(analysis_area_text),
        "",
        glue$glue(
          " This IPC/CH analysis covers a partial area of {location}, {analysis_area_text}.",
          " Refer to the source documents for clarity on coverage and time series ",
          "comparability, as coverage of previous analyses may be different. "
        )
      ),
      other_urls = NA_character_,
      further_information = as.character(
        glue$glue(
          "{partial_coverage_text}",
          'Access the data directly <a href="{hdx_url}">on HDX</a>',
          ", and see the {source_url_text}."
        )
      )
    ) |>
    dplyr$select(
      hdx_url,
      source_url,
      other_urls,
      further_information
    )
}
