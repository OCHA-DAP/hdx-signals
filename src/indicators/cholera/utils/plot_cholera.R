box::use(dplyr)
box::use(purrr)
box::use(rlang[`!!`])
box::use(gghdx)
box::use(gg = ggplot2)
box::use(scales)

box::use(../../../utils/format_date)

gghdx$gghdx()

# local modules
box::use(../../../email/mailchimp/images)

#' Plot all WHO cholera cases
#'
#' Calls `plot_timeline` for all rows in the alerts data frame `df`, controlling
#' for errors.
#'
#' @param df Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df, df_wrangled, preview = FALSE) {
  poss_plot <- purrr$possibly(plot_timeline, data.frame(id = "ERROR", url = "ERROR"))
  df <- dplyr$left_join(
    df,
    df_wrangled,
    by = c("iso3", "date")
  ) |>
    dplyr$mutate(
      plot_title = paste0(
        scales::label_comma()(value),
        " cases of cholera reported since ",
        format_date$format_date(start_date)
      )
    )

  cholera_plot <- purrr$pmap(
    .l = df,
    .f = \(iso3, plot_title, date, ...) poss_plot(
      iso3 = iso3,
      plot_title = plot_title,
      date = date,
      df = df_wrangled,
      preview = preview
    )
  ) |>
    purrr$list_rbind()

  names(cholera_plot) <- paste0("plot_", names(cholera_plot))
  dplyr$bind_cols(dplyr$select(df, plot_title), cholera_plot)
}

#' Plot WHO cholera data
#'
#' Plots cholera data for a specific country, defined by an ISO3 code.
#' The wrangled cholera data is loaded in and filtered to that country.
#' Cholera is plotted as weekly cases, and the background is light red whenever
#' cases are above 1,000.
#'
#' @param iso3 ISO3 code.
#' @param plot_title Plot title.
#' @param date Date for file naming.
#' @param df_wrangled Data frame to plot (wrangled data).
#' @param preview Whether or not to preview the plot
#'
#' @returns Plot of cholera for that country.
plot_timeline <- function(
    iso3, plot_title, date, df_wrangled, preview = FALSE
) {
  # load in the data for plotting
  df_plot <- df_wrangled |>
    dplyr$filter(
      iso3 == !!iso3
    )

  # filter the data to the date if it's a historic campaign (older than 90 days)
  if (Sys.Date() - date > 90) {
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
      title = plot_title,
      caption = "Data from the WHO AFRO, https://www.afro.who.int"
    )

  images$mc_upload_plot(
    plot = p,
    name = paste(iso3, "cholera", format(date, "%Y_%m_%b.png"), sep = "_"),
    folder = "HDX Signals - Cholera",
    preview = preview,
    width = 4,
    height = 3,
    units = "in",
    dpi = 30
  )
}
