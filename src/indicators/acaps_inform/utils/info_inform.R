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
    dplyr$left_join(
      dplyr$distinct(
        df_wrangled, iso3
      ),
      by = "iso3"
    ) |>
    dplyr$transmute(
      hdx_url = "https://data.humdata.org/dataset/inform-global-crisis-severity-index",
      source_url = "https://www.acaps.org/en/",
      other_urls = NA_character_,
      further_information = as.character(
        glue$glue(
          'Access INFORM data directly <a href="{hdx_url}">on HDX</a>. ',
          "For more granular data, analysis, and context, visit the ",
          '<a href="{source_url}">ACAPS website</a>.'
        )
      )
    )
}
