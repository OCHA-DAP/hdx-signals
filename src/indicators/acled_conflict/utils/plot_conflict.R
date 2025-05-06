box::use(
  dplyr,
  lubridate,
  gghdx
)

box::use(
  src/utils/formatters,
  src/images/plots/plot_ts,
  src/images/plots/caption,
  src/images/create_images
)

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
        gghdx$format_number_hdx(value),
        " fatalities since ",
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
#' Plots conflict data for a specific location, defined by an ISO3 code.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame, not used for plotting conflict TS
#' @param title Plot title.
#' @param date Date of the alert
#'
#' @returns Plot of cholera for that wrangled data
#' @export
conflict_ts <- function(df_wrangled, df_raw, title, date, alerts=NA) {
  caption <- caption$caption(
    indicator_id = "acled_conflict",
    iso3 = unique(df_wrangled$iso3)
  )

  # filter conflict data to the latest day of the week, since we are plotting the
  # monthly rolling sum
  df_plot <- dplyr$filter(
    df_wrangled,
    !is.na(fatalities_30d),
    (as.numeric(date - !!date) %% 30) == 0 # every 30 days from date of signal
  )
  browser()
  if (is.data.frame(alerts)){
    df_wrangled_alerting <- df_wrangled |> dplyr$filter(date %in% alerts$date)
    df_plot <- dplyr$bind_rows(
      df_plot,
      df_wrangled_alerting
    )
  }

  plot_ts$plot_ts(
    df = df_plot,
    val_col = "fatalities_30d",
    y_axis = "Fatalities (monthly)",
    title = title,
    caption = caption,
    alerts = alerts
  )
}
