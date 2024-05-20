box::use(dplyr)
box::use(scales)

#' Add campaign info to market monitor alerts
#'
#' @returns Data frame with campaign information
#'
#' @export
summary <- function(df_alerts, df_wrangled, df_raw) {
  df_alerts |>
    dplyr$transmute(
      summary_short = paste0(
        scales$label_percent(
          accuracy = 1,
          scale = 1
        )(value),
        " increase in the cost of the food basket"
      ),
      summary_long = NA_character_
    )
}
