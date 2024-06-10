box::use(gghdx)
box::use(gg = ggplot2)
box::use(showtext)

gghdx$gghdx()

#' HDX Signals theme
#'
#' Creates the theme for HDX Signals. Based off of [gghdx::gghdx()] but adds
#' adjustments to some of the text sizing.
#'
#' @export
theme_signals <- function() {
  showtext$showtext_opts(dpi = 300)

  gghdx$theme_hdx() +
    gg$theme(
      axis.text.x = gg$element_text(vjust = 1),
      plot.title = gg$element_text(size = 14),
      axis.title = gg$element_text(size = 12),
      axis.text = gg$element_text(size = 11),
      plot.caption = gg$element_text(size = 8, hjust = 1, margin = gg$margin(t = 0.15, unit = "in")),
      plot.caption.position = "plot",
      legend.text = gg$element_text(size = 9),
      legend.title = gg$element_text(size = 11),
      panel.background = gg$element_rect(fill = "white", linewidth = 0),
      plot.background = gg$element_rect(fill = "white", linewidth = 0),
    )
}
