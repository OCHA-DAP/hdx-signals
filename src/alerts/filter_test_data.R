box::use(dplyr)
box::use(rlang[`!!`])

#' Filter data for signals testing
#'
#' Filters `df_wrangled` to a small subset of ISO3 codes if `test`
#' is `TRUE`. The ISO3 codes are chosen at random if `test_filter` is `NULL`,
#' otherwise you can pass through a vector of ISO3 codes.
#'
#' @param df_wrangled Wrangled data frame
#' @param test Whether or not this is for testing signals development. If `FALSE`,
#'     the data frames are directly returned.
#' @param test_filter Only used if `test` is `TRUE`. If `NULL`, 2 random ISO3
#'     codes are selected from `df_wrangled` and both data frames are filtered
#'     to it. Otherwise, if a vector of ISO3 codes, data frames are filtered to
#'     those.
#'
#' @export
filter_test_data <- function(df_wrangled, test, test_filter) {
  if (!test) {
    return(
      df_wrangled
    )
  }

  if (is.null(test_filter)) {
    iso3 <- sample(unique(df_wrangled$iso3), size = 2)
  } else {
    iso3 <- test_filter
  }

  dplyr$filter(
    df_wrangled,
    iso3 == !!iso3
  )
}
