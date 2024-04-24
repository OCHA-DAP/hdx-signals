box::use(dplyr)
box::use(glue)

#' Add campaign info to cholera alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
info <- function(df_alerts, df_wrangled) {
  dplyr$tibble(
    hdx_url = NA_character_,
    source_url = "https://acleddata.com",
    other_urls = NA_character_,
    further_information = as.character(
      glue$glue(
        'Refer to the <a href="{source_url}">WHO Afro Bulletins for more detailed information.</a>'
      )
    ),
    .rows = nrow(df)
  )
}
