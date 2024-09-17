box::use(dplyr)

box::use(
  src/utils/hs_dry_run,
  src/utils/hs_first_run
)

#' Filter data for signals testing
#'
#' Filters `df_wrangled` to a small subset of ISO3 codes if `HS_DRY_RUN`
#' is `TRUE`. The ISO3 codes are chosen at random if `dry_run_filter` is `NULL`,
#' otherwise you can pass through a vector of ISO3 codes.
#'
#' If `HS_FIRST_RUN` is `TRUE` and `dry_run_filter` is `NULL`, generates the
#' entire first run.
#'
#' @param df_wrangled Wrangled data frame
#' @param dry_run_filter Only used if `dry_run` is `TRUE`. If `NULL`, 2 random ISO3
#'     codes are selected from `df_wrangled` and both data frames are filtered
#'     to it. Otherwise, if a vector of ISO3 codes, data frames are filtered to
#'     those.
#'
#' @export
filter_test_data <- function(df_wrangled, dry_run_filter) {
  if (!hs_dry_run$hs_dry_run()) {
    return(
      df_wrangled
    )
  }

  if (is.null(dry_run_filter)) {
    iso3 <- unique(df_wrangled$iso3)
    if (!hs_first_run$hs_first_run()) {
      iso3 <- sample(unique(df_wrangled$iso3), size = 2)
    }
  } else {
    iso3 <- dry_run_filter
  }

  dplyr$filter(
    df_wrangled,
    iso3 %in% !!iso3
  )
}
