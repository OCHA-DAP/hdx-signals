box::use(dplyr)
box::use(rlang[`!!`])

box::use(src/utils/alert_daily_ts)

#' Creates displacement alerts dataset
#'
#' Creates base alert dataset for displacement. Uses `alert_daily_ts()` twice
#' to create alerts for when the data is
#'
#' @param df_wrangled Wrangled dataframe
#'
#' @returns Alerts dataset
alert <- function(df_wrangled) {
  if (unique(df_wrangled$displacement_type) == "Conflict") {
    min_val <- 10000
    indicator_name <- "displacement_conflict"
  } else {
    min_val <- 100000
    indicator_name <- "displacement_disaster"
  }

  alert_daily_ts$alert_daily_ts(
    df = df_wrangled,
    val_col = "displacement_30d",
    min_val = min_val
  ) |>
    dplyr$mutate(
      indicator_name = !!indicator_name,
      indicator_source = "idmc",
      indicator_id = paste(indicator_source, indicator_name, sep = "_"),
      .after = iso3
    )
}
