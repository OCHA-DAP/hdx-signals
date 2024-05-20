box::use(dplyr)
box::use(scales)
box::use(gg = ggplot2)
box::use(gghdx)
box::use(lubridate)

box::use(../../../utils/country_codes)
box::use(../../../utils/formatters)
box::use(../../../images/plots/theme_signals)
box::use(../../../images/create_images)

#' Plot JRC ASAP
#'
#' Creates time series of JRC ASAP
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  # add title for use in the plot
  df_plot <- df_alerts |>
    dplyr$mutate(
      title = paste(
        ifelse(
          value == 1,
          "Hotspot",
          "Major hotspot"
        ),
        "declared",
        format(date, "%B %Y")
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = drought_ts,
    image_use = "plot",
    height = 3,
    width = 6
  )
}

#' Plot JRC drought data
#'
#' Plots JRC drought data for a specific country.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
drought_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- paste(
    "Data from JRC ASAP, https://agricultural-production-hotspots.ec.europa.eu",
    paste("Created", formatters$format_date(Sys.Date())),
    country_codes$iso3_to_names(unique(df_wrangled$iso3)),
    sep = "\n"
  )

  df_plot <- df_wrangled |>
    dplyr$mutate(
      year = lubridate$year(date),
      month = lubridate$month(date, label = TRUE)
    ) |>
    dplyr$filter(
      max(year) - year < 5
    )

  df_plot |>
    gg$ggplot(
      mapping = gg$aes(
        x = month,
        y = year,
        fill = hs_name
      )
    ) +
    gg$geom_tile(
      color = gghdx$hdx_hex("grey-dark")
    ) +
    gg$scale_x_discrete(
      breaks = c("Jan", "Apr", "Jul", "Oct")
    ) +
    gg$scale_y_reverse(
      breaks = unique(df_plot$year),
    ) +
    theme_signals$theme_signals() +
    gg$theme(
      legend.position = "none"
    ) +
    gg$scale_fill_manual(
      values = c(
        "No hotspot" = "#FFFFFF",
        "Hotspot" = gghdx$hdx_hex("sapphire-light"),
        "Major hotspot" = gghdx$hdx_hex("sapphire-hdx")
      )
    ) +
    gg$labs(
      x = "",
      y = "",
      fill = "",
      title = title,
      caption = caption
    ) +
    gg$coord_equal() +
    gg$guides(
      fill = gg$guide_legend(byrow = TRUE)
    ) +
    gg$theme(
      panel.grid = gg$element_blank(),
      legend.position = "left",
      legend.direction = "vertical",
      legend.spacing.x = gg$unit(x = 0.1, units = "in"),
      axis.line.x = gg$element_blank()
    )
}
