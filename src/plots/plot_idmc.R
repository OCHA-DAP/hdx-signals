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

plot_idmc <- function(
    iso3,
    country
) {

  # load in the data for plotting
  df_plot <- read_gs_file(
    "wrangled_idmc",
    col_types = "cccDdddddllllllllllldl"
  ) %>%
    filter(
      iso3 == !!iso3
    )

  # generate flagging times for plotting
  df_flag_times <- df_plot %>%
    mutate(
      flag_group = cumsum(!flag_any)
    ) %>%
    filter(
      flag_any
    ) %>%
    group_by(
      flag_group
    ) %>%
    summarize(
      flag_start = min(date),
      flag_end = max(date),
      .groups = "drop"
    )

  df_plot %>%
    mutate(
      year = lubridate::year(date),
      week = lubridate::week(date)
    ) %>%
    group_by(
      year,
      week
    ) %>%
    summarize(
      date = min(date),
      displacement = sum(displacement_daily),
      .groups = "drop"
    ) %>%
    ggplot() +
    geom_rect(
      data = df_flag_times,
      mapping = aes(
        xmin = flag_start,
        xmax = flag_end
      ),
      ymin = 0,
      ymax = Inf,
      fill = hdx_hex("tomato-ultra-light")
    ) +
    geom_line(
      aes(
        x = date,
        y = displacement
      )
    ) +
    expand_limits(y = 0) +
    scale_y_continuous(
      labels = scales::label_comma()
    ) +
    scale_x_date(
      date_breaks = "6 months",
      labels = scales::label_date_short()
    ) +
    theme(
      axis.text.x = element_text(vjust = 1)
    ) +
    labs(
      x = "",
      y = "Displacement (weekly)",
      color = "",
      title = "Historical weekly displacement",
      subtitle = "Abnormally high displacement highlighted in light red",
      caption = "Data from the IDMC, http://www.internal-displacement.org"
    )
}
