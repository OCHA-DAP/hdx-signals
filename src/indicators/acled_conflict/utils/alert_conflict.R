box::use(dplyr)

box::use(src/utils/alert_daily_ts)

#' Creates cholera alerts dataset
#'
#' Creates base alert dataset for cholera. Alerts are generated when
#' cases cross 1,000 or 5,000 from below.
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
#'
#' @export
alert <- function(df_wrangled) {
  alert_daily_ts$alert_daily_ts(
    df = df_wrangled,
    val_col = "fatalities_30d",
    min_val = 100
  ) |>
    dplyr$mutate(
      indicator_name = "conflict",
      indicator_source = "acled",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      exterme_case = FALSE,
      .after = iso3
    )
}
