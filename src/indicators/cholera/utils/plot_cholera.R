box::use(dplyr)
box::use(tidyr)
box::use(rlang[`!!`])
box::use(gghdx)
box::use(gg = ggplot2)
box::use(scales)
box::use(lubridate)

gghdx$gghdx()

# local modules
box::use(cs = ../../../utils/cloud_storage)
box::use(../../../utils/gmas_test_run)
box::use(../../../email/mailchimp/images)

#' Plot WHO cholera data
#'
#' Plots cholera data for a specific country, defined by an ISO3 code.
#' The wrangled cholera data is loaded in and filtered to that country.
#' Cholera is plotted as weekly cases, and the background is light red whenever
#' cases are above 1,000.
#'
#' @param iso3 ISO3 code.
#' @param title Plot title.
#' @param date Date for file naming.
#' @param df Data frame to plot (wrangled data).
#' @param date_filter Filter data frame to on or before `date`. Useful for
#'     producing plots for "historic" alerts.
#' @param id Mailchimp file ID
#'
#' @returns Plot of cholera for that country.
#'
#' @export
plot_timeline <- function(
    iso3, title, date, df, date_filter = FALSE, id = NULL
) {

  # load in the data for plotting
  df_plot <- df |>
    dplyr$filter(
      iso3 == !!iso3
    )

  # filter to end date, only used if passed in, for building out historic plot database
  if (!is.null(date)) {
    df_plot <- dplyr$filter(df_plot, date <= !!date)
  }

  p <- df_plot |>
    gg$ggplot() +
    gg$geom_line(
      gg$aes(
        x = date,
        y = cholera_cases
      )
    ) +
    gghdx$scale_y_continuous_hdx(
      labels = scales$label_comma(),
    ) +
    gg$scale_x_date(
      breaks = scales$pretty_breaks(),
      labels = scales$label_date_short()
    ) +
    gg$coord_cartesian(
      clip = "off"
    ) +
    gg$theme(
      axis.text.x = gg$element_text(vjust = 1),
      plot.title = gg$element_text(size = 24),
      axis.title = gg$element_text(size = 18),
      axis.text = gg$element_text(size = 14),
      plot.caption = gg$element_text(size = 14, hjust = 1)
    ) +
    gg$labs(
      x = "",
      y = "Cholera cases",
      color = "",
      title = title,
      caption = "Data from the WHO AFRO, https://www.afro.who.int"
    )

  images$mc_upload_plot(
    plot = p,
    name = paste(iso3, "cholera", format(date, "%Y_%m_%b.png"), sep = "_"),
    folder = "HDX Signals - Cholera",
    id = id,
    width = 3,
    height = 2,
    dpi = 300
  )
}
