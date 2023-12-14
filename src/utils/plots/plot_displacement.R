box::use(dplyr)
box::use(lubridate)
box::use(rlang[`!!`])
box::use(gghdx)
box::use(gg = ggplot2)
box::use(scales)

gghdx$gghdx()

# local modules
box::use(cs = ../../utils/cloud_storage)

#' Plot IDMC displacement data
#'
#' Plots IDMC displacement data for a specific country, defined by an ISO3 code.
#' The wrangled IDMC displacement data is loaded in and filtered to that country.
#' Displacement is summed to individual weeks and then plotted as a line plot,
#' with any times that alerts were generated color in light red of the
#' background of the plot
#'
#' @param iso3 ISO3 code.
#'
#' @returns Plot of displacement for that country.
#'
#' @export
plot_displacement <- function(
    iso3
) {

  # load in the data for plotting
  df_plot <- cs$read_gcs_file(
    "output/idmc/wrangled.parquet"
  ) |>
    dplyr$filter(
      iso3 == !!iso3
    )

  # generate flagging times for plotting, to create for each row groups of
  # consecutive days where a country was actively flagged
  df_flag_times <- df_plot |>
    dplyr$mutate(
      flag_group = cumsum(!alert_any)
    ) |>
    dplyr$filter(
      alert_any
    ) |>
    dplyr$group_by(
      flag_group
    ) |>
    dplyr$summarize(
      flag_start = min(date),
      flag_end = max(date),
      .groups = "drop"
    )

  df_plot |>
    dplyr$mutate(
      year = lubridate$year(date),
      week = lubridate$week(date)
    ) |>
    dplyr$group_by(
      year,
      week
    ) |>
    dplyr$summarize(
      date = min(date),
      displacement = sum(displacement_daily),
      .groups = "drop"
    ) |>
    gg$ggplot() +
    gg$geom_rect(
      data = df_flag_times,
      mapping = gg$aes(
        xmin = flag_start,
        xmax = flag_end
      ),
      ymin = 0,
      ymax = Inf,
      fill = gghdx$hdx_hex("tomato-ultra-light")
    ) +
    gg$geom_line(
      gg$aes(
        x = date,
        y = displacement
      )
    ) +
    gg$expand_limits(y = 0) +
    gg$scale_y_continuous(
      labels = scales$label_comma()
    ) +
    gg$scale_x_date(
      date_breaks = "6 months",
      labels = scales$label_date_short()
    ) +
    gg$theme(
      axis.text.x = gg$element_text(vjust = 1)
    ) +
    gg$labs(
      x = "",
      y = "Displacement (weekly)",
      color = "",
      title = "Historical weekly displacement",
      subtitle = "Abnormally high displacement highlighted in light red",
      caption = "Data from the IDMC, http://www.internal-displacement.org"
    )
}
