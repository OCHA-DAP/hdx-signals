box::use(src/images/create_images)

#' Plot SEA5 anomaly data
#'
#' Placeholder plot function. No plot implemented yet.
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw Raw data
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  create_images$create_images(
    df_alerts = df_alerts,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = sea5_anomaly_plot,
    image_use = "plot"
  )
}

#' SEA5 anomaly plot (placeholder)
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame.
#' @param title Plot title.
#' @param date Date of the alert.
#'
#' @returns NULL (not yet implemented)
sea5_anomaly_plot <- function(df_wrangled, df_raw, title, date) {
  NULL
}
