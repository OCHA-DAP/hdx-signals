box::use(
  gghdx,
  gg = ggplot2,
  showtext,
  sysfonts,
  rlang
)

box::use(
  src/images/plots/hdx_signals_palette
)

gghdx$gghdx()

# HDX 2025 redesign fonts: Merriweather for titles, Roboto for everything
# else, Roboto Mono for numeric legend/axis values. Loaded alongside (not
# replacing) gghdx's Source Sans 3 since gghdx still depends on it
# internally (e.g. geom_text_hdx()).
sysfonts$font_add_google("Merriweather", "Merriweather")
sysfonts$font_add_google("Roboto", "Roboto")
sysfonts$font_add_google("Roboto Mono", "Roboto Mono")
showtext$showtext_auto()

#' HDX Signals theme
#'
#' Creates the theme for HDX Signals. Based off of [gghdx::gghdx()] but adds
#' adjustments to some of the text sizing and margins.
#'
#' @param margin_location Where to place the margins in the plot. Don't want to
#' put margins after title if subtitle exists, because we would want to place
#' the margins between subtitle and plot.
#' @param axis_ticks Whether or not to place axis ticks on the x-axis. Used for
#'     some time series.
#'
#' @export
theme_signals <- function(margin_location = c("title", "subtitle"), x_axis_ticks = FALSE) {
  margin_location <- rlang$arg_match(margin_location)
  showtext$showtext_opts(dpi = 300)

  # use different margins depending on if subtitle or title is passed
  # HDX 2025 redesign: display serif for titles, Roboto for everything else
  title_family <- "Merriweather"
  body_family <- "Roboto"

  theme_margins <- switch(
    margin_location,
    "title" = gg$theme(
      plot.title = gg$element_text(family = title_family, size = 14, margin = gg$margin(b = 0.2, unit = "in"))
    ),
    "subtitle" = gg$theme(
      plot.title = gg$element_text(family = title_family, size = 14, margin = gg$margin(b = 0.1, unit = "in")),
      plot.subtitle = gg$element_text(family = body_family, margin = gg$margin(b = 0.2, unit = "in"))
    )
  )

  theme_obj <- gghdx$theme_hdx() +
    gg$theme(
      text = gg$element_text(family = body_family),
      axis.text.x = gg$element_text(vjust = 1),
      axis.title = gg$element_text(size = 12, color = hdx_signals_palette$text_muted),
      axis.text = gg$element_text(size = 11, color = hdx_signals_palette$text_muted),
      plot.caption = gg$element_text(size = 8, hjust = 0, margin = gg$margin(t = 0.1, unit = "in")),
      plot.caption.position = "plot",
      legend.text = gg$element_text(size = 9),
      legend.title = gg$element_text(size = 11),
      panel.background = gg$element_rect(fill = "white", linewidth = 0),
      plot.background = gg$element_rect(fill = "white", linewidth = 0),
      panel.grid.major = gg$element_line(color = hdx_signals_palette$hairline),
    ) +
    theme_margins

  if (x_axis_ticks) {
    theme_obj <- theme_obj +
      gg$theme(
        axis.ticks.x.bottom = gg$element_line(
          colour = hdx_signals_palette$hairline,
          linewidth = gg$rel(1)
        ),
        axis.ticks.length = gg$unit(-0.05, "in")
      )
  }

  theme_obj
}
