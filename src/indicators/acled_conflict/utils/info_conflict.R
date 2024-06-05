box::use(dplyr)
box::use(glue)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled, df_raw) {
  df_alerts |>
    dplyr$left_join(
      dplyr$distinct(
        df_wrangled, iso3, acled_hdx_url
      ),
      by = "iso3"
    ) |>
    dplyr$transmute(
      hdx_url = acled_hdx_url,
      source_url = "https://acleddata.com",
      other_urls = NA_character_,
      further_information = as.character(
        glue$glue(
          'Access ACLED conflict data for {location} directly <a href="{hdx_url}">on HDX</a>. ',
          "For more granular data, analysis, and context, visit the ",
          '<a href="{source_url}">ACLED website</a>.'
        )
      )
    )
}
