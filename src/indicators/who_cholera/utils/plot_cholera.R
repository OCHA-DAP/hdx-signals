box::use(dplyr)
box::use(purrr)
box::use(rlang[`!!`])
box::use(scales)

box::use(../../../utils/format_date)
box::use(../../../plots/plot_ts)

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
        scales$label_comma()(value),
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

  p <- plot_ts$plot_ts(
    df = df_plot,
    date = "date",
    values = "cholera_cases",
    y_axis = "Cholera cases",
    title = plot_title,
    subtitle = "",
    caption = "Data from the WHO AFRO, https://www.afro.who.int"
  )

  images$mc_upload_plot(
    plot = p,
    name = paste(iso3, "cholera", format(date, "%Y_%m_%b.png"), sep = "_"),
    folder = "HDX Signals - Cholera",
    preview = preview,
    width = 6,
    height = 3,
    units = "in",
    dpi = 300
  )
}
