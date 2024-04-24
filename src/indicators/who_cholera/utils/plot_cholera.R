box::use(dplyr)
box::use(purrr)
box::use(rlang[`!!`])
box::use(scales)

box::use(../../../utils/format_date)
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
plot <- function(df_alerts, df_wrangled, preview = FALSE) {
  df_plot <- dplyr$left_join(
    df_alerts,
    df_wrangled,
    by = c("iso3", "date")
  ) |>
    dplyr$mutate(
      title = paste0(
        scales$label_comma()(value),
        " cases of cholera reported since ",
        format_date$format_date(start_date)
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    image_fn = cholera_ts,
    image_use = "plot"
  )
}

#' Plot WHO cholera data
#'
#' Plots cholera data for a specific country, defined by an ISO3 code.
#' The wrangled cholera data is loaded in and filtered to that country.
#' Cholera is plotted as weekly cases, and the background is light red whenever
#' cases are above 1,000.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param title Plot title.
#'
#' @returns Plot of cholera for that wrangled data
cholera_ts <- function(
    df_wrangled, title
) {
  plot_ts$plot_ts(
    df = df_wrangled,
    val_col = "cholera_cases",
    y_axis = "Cholera cases",
    title = title,
    subtitle = "",
    caption = "Data from the WHO AFRO, https://www.afro.who.int"
  )
}
