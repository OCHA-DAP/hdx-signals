box::use(dplyr)
box::use(scales)
box::use(gg = ggplot2)
box::use(gghdx)

box::use(../../../utils/country_codes)
box::use(../../../utils/formatters)
box::use(../../../images/create_images)
box::use(../../../images/plots/theme_signals)

#' Plot WFP market monitor food basket price changes
#'
#' Creates time series for WFP market monitor for alert
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  df_plot <- df_alerts |>
    dplyr$mutate(
      title = paste0(
        scales$label_percent(
          accuracy = 1,
          scale = 1
        )(value),
        " increase in the cost of the food basket over the past month"
      )
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = food_basket_ts,
    image_use = "plot"
  )
}

#' Plot WFP food basket prices
#'
#' Plots food basket prices data for a specific country, defined by an ISO3 code.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
food_basket_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- paste(
    "Data from WFP Global Market Monitor, https://www.wfp.org/publications/market-monitor",
    paste("Created", formatters$format_date(Sys.Date())),
    country_codes$iso3_to_names(unique(df_wrangled$iso3)),
    sep = "\n"
  )

  df_plot <- df_wrangled |>
    dplyr$filter(
      max(date) - date <= 365 * 5,
      !is.na(basket_change)
    )

  p <- df_plot |>
    gg$ggplot(
      mapping = gg$aes(
        x = date,
        y = basket_change
      )
    ) +
    gg$geom_bar(
      stat = "identity",
      fill = gghdx$hdx_hex("sapphire-hdx"),
      color = gghdx$hdx_hex("sapphire-hdx") # so 0 values appear
    ) +
    gg$scale_y_continuous(
      labels = scales$label_percent(
        accuracy = 1,
        scale = 1
      ),
    ) +
    gg$scale_x_date(
      breaks = scales$pretty_breaks(),
      labels = scales$label_date_short()
    ) +
    gg$coord_cartesian(
      clip = "off"
    ) +
    theme_signals$theme_signals() +
    gg$labs(
      x = "",
      y = "% change (monthly)",
      title = title,
      subtitle = "",
      caption = caption
    )

  # if the plot is only positive or negative, ensure we expand limits
  if (length(unique(sign(df_plot$basket_change))) == 1) {
    p <- p + gg$expand_limits(y = 0)
  }

  p
}
