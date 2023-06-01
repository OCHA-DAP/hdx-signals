library(tidyverse)
library(gghdx)
gghdx()

source(
  file.path(
    "src",
    "utils",
    "googledrive.R"
  )
)

plot_ipc <- function(
    iso3,
    country
) {

  # load in the data for plotting
  drive_wrangled_ipc <- get_drive_file("wrangled_ipc")
  drive_download(drive_wrangled_ipc, path = f <- tempfile(fileext = ".csv"))
  df_plot <- read_csv(f) %>%
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

  mid_date <- mean(range(df_proj$analysis_period_start, df_proj$analysis_period_end))
  # midway after
  start_date <- mean(c(min(df_proj$analysis_period_start), min(df_latest_curr$analysis_period_start)))

  df_plot %>%
    ggplot(
      aes(
        x = analysis_period_start,
        y = value,
        color = name
      )
    ) +
    geom_rect(
      data = df_proj,
      mapping = aes(
        xmax = analysis_period_end
      ),
      xmin = start_date,
      ymin = 0,
      ymax = Inf,
      color = NA,
      fill = hdx_hex("mint-hdx")
    ) +
    geom_text_hdx(
      aes(
        y = mean(range(value)),
      ),
      x = mid_date,
      label = "Projection period",
      angle = -90,
      color = "white",
      size = 3
    ) +
    geom_point() +
    geom_line() +
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
