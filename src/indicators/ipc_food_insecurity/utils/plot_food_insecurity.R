box::use(dplyr)
box::use(tidyr)
box::use(scales)
box::use(gg = ggplot2)
box::use(gghdx)
box::use(lubridate)
box::use(readr)
box::use(ggrepel)
box::use(utils)
box::use(glue)
box::use(logger)

box::use(../../../images/plots/theme_signals)
box::use(../../../images/plots/caption)
box::use(../../../images/create_images)

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
  df_plot <- df_alerts |>
    dplyr$mutate(
      title = paste0(
        scales$label_percent(accuracy = 1)(value),
        " of the ",
        type,
        " analyzed population in P",
        phase_level
      )
    )

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
#' Plots food insecurity data for a specific location.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
food_insecurity_ts <- function(df_wrangled, df_raw, title, date) {
  caption <- caption$caption(
    indicator_id = "ipc_food_insecurity",
    iso3 = unique(df_wrangled$iso3)
  )

  # only plot the points that are comparable to the latest
  df_plot <- df_wrangled |>
    dplyr$group_by(
      phase
    ) |>
    dplyr$mutate( # only keep rows that are comparable
      compare_filter = cumsum(
        compare_current != dplyr$lead(compare_current, default = utils$tail(compare_current, n = 1))
      )
    ) |>
    dplyr$ungroup() |>
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

  # subtitle for plots to highlight coverage
  analysis_area <- utils$tail(df_plot$analysis_area, n = 1)
  if (is.na(analysis_area)) {
    analysis_area_txt <- ""
  } else {
    analysis_area_txt <- glue$glue("{analysis_area}, ")
  }
  coverage <- utils$tail(df_plot$coverage, n = 1) |>
    scales$label_percent(accuracy = 1)()

  subtitle <- glue$glue(
    "{analysis_area_txt}{coverage} population coverage in latest analysis"
  )

  # only plot the current values in solid lines
  df_current <- df_plot |>
    dplyr$filter(
      type == "current"
    )

  # plot the projected values in dotted lines
  df_projected <- df_plot |>
    dplyr$filter(
      date == max(date, as.Date("1500-01-01"))
    )

  # get the phase text for the dataset
  df_phase_labels <- df_projected |>
    dplyr$filter(
      plot_date == max(plot_date, as.Date("1500-01-01"))
    ) |>
    dplyr$mutate(
      label = paste0("P", readr$parse_number(phase)),
      plot_date = plot_date + lubridate$month(6)
    )

  p <- gg$ggplot(
    mapping = gg$aes(
      x = plot_date,
      y = percentage,
      color = phase
    )
  )

  # only add current line if sufficient data (more than one analysis available)
  if (length(unique(df_current$plot_date)) > 1) {
    p <- p +
      gg$geom_line(
        data = df_current,
        linewidth = 0.7
      )
  }

  # some analyses don't have projections, so don't plot line or points in this case
  if (length(unique(df_projected$type)) > 1) {
    p <- p +
      gg$geom_line(
        data = df_projected,
        linetype = 2,
        linewidth = 0.7
      ) +
      gg$geom_point(
        data = df_projected,
        size = 3
      )
  }

  p +
    gg$geom_point(
      data = dplyr$filter(df_current, plot_date == max(plot_date, as.Date("1500-01-01"))),
      size = 3
    ) +
    ggrepel$geom_text_repel(
      data = df_phase_labels,
      mapping = gg$aes(
        label = label
      ),
      nudge_x = 1,
      segment.color = NA
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
    theme_signals$theme_signals(
      margin_location = "subtitle",
      x_axis_ticks = TRUE
    ) +
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
      subtitle = subtitle,
      caption = caption
    )
}
