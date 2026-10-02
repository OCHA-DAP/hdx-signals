box::use(dplyr)

#' Wrangle SEAS5 anomaly data
#'
#' Adds the SEAS5 issuance `date` (first of `issued_month` in `issued_year`),
#' which the signals framework uses to filter and join alerts, converts the
#' dictionary-encoded `iso3` and `name` columns from factor to character and
#' drops the pandas index column written by the upstream pipeline. All other
#' columns keep the names they have in the source file.
#'
#' @param df_raw Raw SEAS5 anomaly data frame
#'
#' @returns Wrangled data frame, one row per (`pcode`, `trimester`)
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    dplyr$mutate(
      dplyr$across(c(iso3, name), as.character),
      date = as.Date(sprintf("%d-%02d-01", issued_year, issued_month)),
      .after = issued_month
    ) |>
    dplyr$select(-dplyr$any_of("__index_level_0__"))
}
