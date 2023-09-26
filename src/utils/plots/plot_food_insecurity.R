box::use(dplyr)
box::use(tidyr)
box::use(rlang[`!!`])
box::use(gghdx)
box::use(gg = ggplot2)
box::use(scales)

gghdx$gghdx()

# local modules
box::use(gs = ../../utils/google_sheets)

#' Plot IPC food insecurity data
#'
#' Plots IPC food insecurity data for a specific country, defined by an ISO3 code.
#' The wrangled IPC data is loaded in and filtered to that country.
#' Food insecurity is plotted as population % in phases 3+, 4+, and 5. Points
#' are added to the current and projected estimates that generated the alerts
#' (i.e. those that saw a rise in estimates). Colored areas indicate the
#' current estimates and projected estimates in the plot.
#'
#' @param iso3 ISO3 code.
#'
#' @returns Plot of food insecurity for that country.
#'
#' @export
plot_food_insecurity <- function(
    iso3
) {
  # load in the data for plotting
  df_plot <- gs$read_gs_file("wrangled_ipc") |>
    dplyr$filter(
      iso3 == !!iso3
    ) |>
    tidyr$pivot_longer(
      cols = c(phase_3pl_pct, phase_4pl_pct, phase_5_pct)
    ) |>
    dplyr$mutate(
      type_label = ifelse(
        analysis_type == "current",
        "estimate",
        "projection"
      )
    )

  # get date for plotting text areas
  df_proj <- df_plot |>
    dplyr$filter(
      analysis_type != "current"
    )

  df_latest_curr <- df_plot |>
    dplyr$filter(
      analysis_type == "current"
    ) |>
    dplyr$filter(
      analysis_period_start == max(analysis_period_start)
    )

  # get the start of the previous current estimate
  df_prev_curr <- df_plot |>
    dplyr$filter(
      analysis_type == "current"
    ) |>
    dplyr$filter(
      analysis_period_start != max(analysis_period_start)
    ) |>
    dplyr$filter(
      analysis_period_start == max(analysis_period_start)
    )

  # find the start dates of the areas
  start_date_curr <- mean(c(min(df_prev_curr$analysis_period_start), min(df_latest_curr$analysis_period_start)))
  start_date_proj <- mean(c(min(df_proj$analysis_period_start), min(df_latest_curr$analysis_period_start)))

  # find the mid dates of the text
  mid_date_proj <- mean(c(start_date_proj, max(df_proj$analysis_period_end)))
  mid_date_curr <- mean(c(start_date_curr, max(df_latest_curr$analysis_period_end)))

  # only add in points for the increases for highlighting
  df_point <- df_plot |>
    dplyr$filter(
      phase_3pl_pct_delta > 0 | phase_4pl_pct_delta > 0 | phase_5_pct_delta > 0,
      date_of_analysis == max(date_of_analysis)
    )

  df_plot |>
    gg$ggplot(
      gg$aes(
        x = analysis_period_start,
        y = value,
        color = name
      )
    ) +
    gg$geom_rect(
      xmin = start_date_curr,
      xmax = start_date_proj,
      ymin = 0,
      ymax = Inf,
      color = NA,
      fill = gghdx$hdx_hex("mint-light")
    ) +
    gg$geom_rect(
      data = df_proj,
      mapping = gg$aes(
        xmax = analysis_period_end
      ),
      xmin = start_date_proj,
      ymin = 0,
      ymax = Inf,
      color = NA,
      fill = gghdx$hdx_hex("sapphire-light")
    ) +
    gghdx$geom_text_hdx(
      gg$aes(
        y = mean(range(value)),
      ),
      x = mid_date_proj,
      label = "Projections",
      angle = -90,
      color = "white",
      size = 3.5
    ) +
    gghdx$geom_text_hdx(
      gg$aes(
        y = mean(range(value)),
      ),
      x = mid_date_curr,
      label = "Current estimate",
      angle = -90,
      color = "white",
      size = 3.5
    ) +
    gg$geom_line() +
    gg$geom_point(
      data = df_point,
      shape = 21,
      fill = "white",
      color = "black"
    ) +
    gg$expand_limits(y = 0) +
    gg$scale_y_continuous(
      labels = scales$label_percent()
    ) +
    gg$scale_x_date(
      date_breaks = "6 months",
      labels = scales$label_date_short()
    ) +
    gg$scale_color_manual(
      values = c("#E67800", "#C80000", "#640000"),
      labels = c("Phase 3+", "Phase 4+", "Phase 5")
    ) +
    gg$theme(
      axis.text.x = gg$element_text(vjust = 1),
      legend.box.margin = gg$margin(t = -25)
    ) +
    gg$labs(
      x = "",
      y = "Analyzed population (%)",
      color = "",
      title = "Acute food insecurity, % of population",
      caption = "Data from the CH-IPC API, https://www.ipcinfo.org/ipc-country-analysis/api/"
    )
}
