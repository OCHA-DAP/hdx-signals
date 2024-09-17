box::use(
  dplyr,
  glue
)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df, df_wrangled, df_raw) {
  dplyr$tibble(
    hdx_url = NA_character_,
    source_url = "https://www.afro.who.int/health-topics/disease-outbreaks/outbreaks-and-other-emergencies-updates",
    other_urls = NA_character_,
    further_information = as.character(
      glue$glue(
        'Refer to the <a href="{source_url}">WHO Afro Bulletins for more detailed information.</a>'
      )
    ),
    .rows = nrow(df)
  )
}
