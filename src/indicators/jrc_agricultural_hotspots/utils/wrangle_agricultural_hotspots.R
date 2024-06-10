box::use(dplyr)

#' Wrangle drought data
#'
#' Does very little, as data is essentially ready for alerting and plotting. For
#' now adds an ISO3 code.
#'
#' @param df_raw Raw drought data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    dplyr$transmute(
      iso3,
      date,
      date_label = format(date, "%B %Y"),
      comment,
      hs_code,
      hs_name
    ) |>
    dplyr$arrange(
      iso3,
      date
    )
}
