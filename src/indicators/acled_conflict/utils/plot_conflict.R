box::use(dplyr)
box::use(purrr)
box::use(rlang[`!!`])
box::use(scales)
box::use(lubridate)

box::use(../../../utils/country_codes)
box::use(../../../utils/formatters)
box::use(../../../images/plots/plot_ts)
box::use(../../../images/create_images)

#' Plot WHO cholera cases
#'
#' Creates time series of cholera cases for each alerts
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  df_plot <- df_alerts |>
    dplyr$mutate(
      title = paste0(
        scales$label_comma()(value),
        " conflict fatalities since ",
        formatters$format_date(date - lubridate$days(30))
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    image_fn = conflict_ts,
    image_use = "plot"
  )
}

#' Plot ACLED conflict data
#'
#' Plots conflict data for a specific country, defined by an ISO3 code.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame, not used for plotting conflict TS
#' @param title Plot title.
#' @param date Date of the alert
#'
#' @returns Plot of cholera for that wrangled data
conflict_ts <- function(
    df_wrangled, df_raw, title, date
) {
  caption <- paste(
    "Data from the Armed Conflict Location & Event Data Project",
    paste0("Accessed ", formatters$format_date(Sys.Date()), ", www.acleddata.com"),
    country_codes$iso3_to_names(unique(df_wrangled$iso3)),
    sep = "\n"
  )

  # filter conflict data to the latest day of the week, since we are plotting the
  # weekly rolling sum
  day_of_week <- lubridate$wday(max(df_wrangled$date))
  df_plot <- dplyr$filter(
    df_wrangled,
    lubridate$wday(date) == day_of_week,
    !is.na(fatalities_7d)
  )

  plot_ts$plot_ts(
    df = df_plot,
    val_col = "fatalities_7d",
    y_axis = "Conflict fatalities (weekly)",
    title = title,
    subtitle = "",
    caption = caption
  )
}
