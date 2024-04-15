box::use(../../../alerts/generate_campaign)

#' Add campaign info to alerts
#'
#' Adds campaign information to alerts dataset, including plots, maps, relevant
#' links and other information to include in the campaign. If `recreate`, plots
#' are recreated with filtering down to the date. Otherwise, plots are created
#' across the full length of the data.
#'
#' @param df_alerts Alerts dataset to create campaign info for
#' @param df_wrangled Wrangled data frame
#' @param recreate Whether or not we are recreating the campaign information, which
#'     determines the timeline of the plots in `plot_timeline()`.
#'
#' @returns Data frame with campaign information
campaign_info <- function(df_alerts, df_wrangled, recreate) {
  # generate plot
  cholera_plot <- purrr$pmap(
    .l = list(
      iso3 = df_alerts$iso3,
      title = df_alerts$message,
      date = df_alerts$date
    ),
    .f = \(iso3, title, date) plot_cholera$plot_timeline(
      iso3 = iso3,
      title = title,
      date = date,
      df = df_wrangled,
      date_filter = recreate
    )
  ) |>
    purrr$list_rbind()

  df_alerts |>
    dplyr$mutate(
      plot = purrr$pmap_chr(
        .l = list(
          iso3 = iso3,
          title = message,
          date = date
        ),
        .f = \(iso3, title, date) plot_cholera$plot_timeline(
          iso3 = iso3,
          title = title,
          date = date,
          df = df_wrangled,
          date_filter = recreate
        )
      ),
      map = NA_character_,
      other_images = NA_character_,
      summary = NA_character_,
      hdx_url = NA_character_,
      source_url = "https://www.afro.who.int/health-topics/disease-outbreaks/outbreaks-and-other-emergencies-updates",
      other_urls = NA_character_,
      further_information = glue$glue(
        'Refer to the <a href="{source_url}">WHO Afro Bulletins for more detailed information.</a>'
      )
    )
}
