box::use(dplyr)
box::use(purrr)
box::use(tidyr)
box::use(stringr)
box::use(rlang[`!!`])
box::use(scales)
box::use(gg = ggplot2)
box::use(gghdx)
box::use(lubridate)
box::use(readr)
box::use(ggrepel)

box::use(../../../utils/country_codes)
box::use(../../../utils/formatters)
box::use(../../../images/plots/theme_signals)
box::use(../../../images/create_images)

box::use(./util_alert_filter)

#' Plot IPC food insecurity
#'
#' Creates time series of food insecurity
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw
#' @param preview Whether or not to preview the plots
#'
#' @export
plot <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  # add title for use in the plot
  df_title <- df_wrangled |>
    dplyr$filter(
      iso3 %in% unique(df_alerts$iso3)
    ) |>
    util_alert_filter$ipc_alert_filter() |>
    dplyr$group_by(iso3) |>
    dplyr$filter(
      date == max(date, as.Date("1500-01-01"))
    ) |>
    dplyr$select(
      iso3,
      phase_level,
      `percentage-current`,
      `percentage-projected`,
      `percentage-second_projected`
    ) |>
    tidyr$pivot_longer(
      dplyr$starts_with("percentage")
    ) |>
    dplyr$filter(
      value == max(value, -Inf, na.rm = TRUE)
    ) |>
    dplyr$slice(1) |>
    dplyr$transmute(
      title = paste0(
        scales$label_percent(accuracy = 1)(value),
        " of the population ",
        if (stringr$str_detect(name, "projected")) "projected" else "estimated",
        " in P",
        if (phase_level == 5) phase_level else paste0(phase_level, "+")
      )
    )

  df_plot <- dplyr$left_join(df_alerts, df_title, by = "iso3")

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = food_insecurity_ts,
    image_use = "plot"
  )
}

#' Plot IPC food insceurity data
#'
#' Plots food insecurity data for a specific country.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
food_insecurity_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- paste(
    "Data from the IPC, https://www.ipcinfo.org",
    paste("Created", formatters$format_date(Sys.Date())),
    country_codes$iso3_to_names(unique(df_wrangled$iso3)),
    sep = "\n"
  )

  df_plot <- df_wrangled |>
    dplyr$filter(
      phase %in% c("phase3", "phase4", "phase5")
    ) |>
    tidyr$pivot_longer(
      cols = dplyr$contains("-"),
      names_sep = "-",
      names_to = c(".value", "type")
    ) |>
    dplyr$filter(
      !is.na(plot_date)
    )

  # only plot the current values in solid lines
  df_current <- df_plot |>
    dplyr$filter(
      type == "current"
    )

  # plot the projected values in dotted lines
  df_projected <- df_plot |>
    dplyr$filter(
      date == max(date)
    )

  # get the text for the dataset
  df_labels <- df_projected |>
    dplyr$filter(
      plot_date == max(plot_date)
    ) |>
    dplyr$mutate(
      label = paste0("P", readr$parse_number(phase)),
      plot_date = plot_date + lubridate$month(6)
    )

  gg$ggplot(
    mapping = gg$aes(
      x = plot_date,
      y = percentage,
      color = phase
    )
  ) +
    gg$geom_line(
      data = df_current,
      linewidth = 0.7
    ) +
    gg$geom_point(
      data = dplyr$filter(df_current, plot_date == max(plot_date)),
      size = 3
    ) +
    gg$geom_line(
      data = df_projected,
      linetype = 2,
      linewidth = 0.7
    ) +
    gg$geom_point(
      data = dplyr$filter(df_projected, plot_date == max(plot_date)),
      size = 3
    ) +
    ggrepel$geom_text_repel(
      data = df_labels,
      mapping = gg$aes(
        label = label
      )
    ) +
    gghdx$scale_y_continuous_hdx(
      labels = scales$label_percent(accuracy = 1),
    ) +
    gg$scale_x_date(
      breaks = scales$pretty_breaks(),
      labels = scales$label_date_short()
    ) +
    gg$coord_cartesian(
      clip = "off"
    ) +
    gg$expand_limits(
      y = 0
    ) +
    theme_signals$theme_signals() +
    gg$theme(
      legend.position = "none"
    ) +
    gg$scale_color_manual(
      values = c(
        "phase3" = "#E67800",
        "phase4" = "#C80000",
        "phase5" = "#640000"
      )
    ) +
    gg$labs(
      x = "",
      y = "% of population",
      title = title,
      subtitle = "",
      caption = caption
    )
}