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

plot_idmc <- function(
    iso3,
    country
) {

  # load in the data for plotting
  drive_wrangled_idmc <- get_drive_file("wrangled_idmc")
  drive_download(drive_wrangled_idmc, path = f <- tempfile(fileext = ".csv"))
  df_plot <- read_csv(f) %>%
    filter(
      iso3 == !!iso3
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
    ggplot(
      aes(
        x = date,
        y = displacement
      )
    ) +
    geom_line() +
    expand_limits(y = 0) +
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
      title = paste0(country, ", IDMC weekly displacement"),
      caption = "Data from the IDMC, http://www.internal-displacement.org"
    )
}
