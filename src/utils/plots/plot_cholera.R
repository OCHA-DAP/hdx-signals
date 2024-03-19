box::use(dplyr)
box::use(tidyr)
box::use(rlang[`!!`])
box::use(gghdx)
box::use(gg = ggplot2)
box::use(scales)

gghdx$gghdx()

# local modules
box::use(cs = ../../utils/cloud_storage)

#' Plot WHO cholera data
#'
#' Plots cholera data for a specific country, defined by an ISO3 code.
#' The wrangled cholera data is loaded in and filtered to that country.
#' Cholera is plotted as weekly cases, and the background is light red whenever
#' cases are above 1,000.
#'
#' @param iso3 ISO3 code.
#'
#' @returns Plot of cholera for that country.
#'
#' @export
plot_cholera <- function(
    iso3
) {

  # load in the data for plotting
  df_plot <- cs$read_az_file("output/cholera/wrangled.parquet") |>
    dplyr$filter(
      iso3 == !!iso3
    )

  # generate flagging times for plotting
  df_flag_times <- df_plot |>
    dplyr$mutate(
      flag_group = cumsum(cholera_cases < 1000)
    ) |>
    dplyr$filter(
      cholera_cases >= 1000
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
        y = cholera_cases
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
      y = "Cholera cases",
      color = "",
      title = "Historical cholera patterns",
      subtitle = "Caseloads above 1,000 highlighted in light red",
      caption = "Data from the WHO AFRO, https://www.afro.who.int"
    )
}
