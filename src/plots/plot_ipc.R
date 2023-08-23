library(tidyverse)
library(gghdx)
gghdx()

source(
  file.path(
    "src",
    "utils",
    "google_sheets.R"
  )
)

plot_ipc <- function(
    iso3,
    country
) {

  # load in the data for plotting
  df_plot <- read_gs_file("wrangled_ipc") %>%
    filter(
      iso3 == !!iso3
    ) %>%
    pivot_longer(
      cols = c(phase_3pl_pct, phase_4pl_pct, phase_5_pct)
    ) %>%
    mutate(
      type_label = ifelse(
        analysis_type == "current",
        "estimate",
        "projection"
      )
    )

  # get date for plotting text areas
  df_proj <- df_plot %>%
    filter(
      analysis_type != "current"
    )

  df_latest_curr <- df_plot %>%
    filter(
      analysis_type == "current"
    ) %>%
    filter(
      analysis_period_start == max(analysis_period_start)
    )

  # get the start of the previous current estimate
  df_prev_curr <- df_plot %>%
    filter(
      analysis_type == "current"
    ) %>%
    filter(
      analysis_period_start != max(analysis_period_start)
    ) %>%
    filter(
      analysis_period_start == max(analysis_period_start)
    )

  # find the start dates of the areas
  start_date_curr <- mean(c(min(df_prev_curr$analysis_period_start), min(df_latest_curr$analysis_period_start)))
  start_date_proj <- mean(c(min(df_proj$analysis_period_start), min(df_latest_curr$analysis_period_start)))

  # find the mid dates of the text
  mid_date_proj <- mean(c(start_date_proj, max(df_proj$analysis_period_end)))
  mid_date_curr <- mean(c(start_date_curr, max(df_latest_curr$analysis_period_end)))

  # only add in points for the increases for highlighting
  df_point <- df_plot %>%
    filter(
      phase_3pl_pct_delta > 0 | phase_4pl_pct_delta > 0 | phase_5_pct_delta > 0,
      date_of_analysis == max(date_of_analysis)
    )

  df_plot %>%
    ggplot(
      aes(
        x = analysis_period_start,
        y = value,
        color = name
      )
    ) +
    geom_rect(
      xmin = start_date_curr,
      xmax = start_date_proj,
      ymin = 0,
      ymax = Inf,
      color = NA,
      fill = hdx_hex("mint-light")
    ) +
    geom_rect(
      data = df_proj,
      mapping = aes(
        xmax = analysis_period_end
      ),
      xmin = start_date_proj,
      ymin = 0,
      ymax = Inf,
      color = NA,
      fill = hdx_hex("sapphire-light")
    ) +
    geom_text_hdx(
      aes(
        y = mean(range(value)),
      ),
      x = mid_date_proj,
      label = "Projections",
      angle = -90,
      color = "white",
      size = 3.5
    ) +
    geom_text_hdx(
      aes(
        y = mean(range(value)),
      ),
      x = mid_date_curr,
      label = "Current estimate",
      angle = -90,
      color = "white",
      size = 3.5
    ) +
    geom_line() +
    geom_point(
      data = df_point,
      shape = 21,
      fill = "white",
      color = "black"
    ) +
    expand_limits(y = 0) +
    scale_y_continuous(
      labels = scales::label_percent()
    ) +
    scale_x_date(
      date_breaks = "6 months",
      labels = scales::label_date_short()
    ) +
    scale_color_manual(
      values = c("#E67800", "#C80000", "#640000"),
      labels = c("Phase 3+", "Phase 4+", "Phase 5")
    ) +
    theme(
      axis.text.x = element_text(vjust = 1),
      legend.box.margin = margin(t = -25)
    ) +
    labs(
      x = "",
      y = "Analyzed population (%)",
      color = "",
      title = "Acute food insecurity, % of population",
      caption = "Data from the CH-IPC API, https://www.ipcinfo.org/ipc-country-analysis/api/"
    )
}
