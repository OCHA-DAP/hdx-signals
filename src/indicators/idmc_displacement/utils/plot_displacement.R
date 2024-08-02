box::use(dplyr)
box::use(gghdx)
box::use(lubridate)

box::use(../../../utils/formatters)
box::use(../../../images/plots/plot_ts)
box::use(../../../images/plots/caption)
box::use(../../../images/create_images)

#' Plot IDMC displacement
#'
#' Creates time series of IDMC displacement for each alerts
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  displacement_cause <- tolower(unique(df_wrangled$displacement_type))

  df_plot <- df_alerts |>
    dplyr$mutate(
      title = paste0(
        gghdx$format_number_hdx(round(value)),
        " internal displacements since ",
        formatters$format_date(date - lubridate$days(30))
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = displacement_ts,
    image_use = "plot"
  )
}

#' Plot IDMC displacement data
#'
#' Plots displacement data for a specific location, defined by an ISO3 code.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
displacement_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- caption$caption(
    indicator_id = "idmc_displacement_conflict", # same details as disaster
    iso3 = unique(df_wrangled$iso3)
  )

  # filter displacement data to the latest day of the week, since we are plotting the
  # weekly rolling sum
  df_plot <- dplyr$filter(
    df_wrangled,
    (as.numeric(date - !!date) %% 30) == 0 # every 30 days from date of signal
  )

  plot_ts$plot_ts(
    df = dplyr$filter(df_plot, !is.na(displacement_30d)),
    val_col = "displacement_30d",
    y_axis = "Displacements (monthly)",
    title = title,
    subtitle = paste0(unique(df_wrangled$displacement_type), "-driven displacements"),
    caption = caption
  )
}
