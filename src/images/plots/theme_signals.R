box::use(gghdx)
box::use(gg = ggplot2)
box::use(showtext)
box::use(rlang)

gghdx$gghdx()

#' HDX Signals theme
#'
#' Creates the theme for HDX Signals. Based off of [gghdx::gghdx()] but adds
#' adjustments to some of the text sizing and margins.
#'
#' @export
theme_signals <- function(margin_location = c("title", "subtitle")) {
  margin_location <- rlang$arg_match(margin_location)
  showtext$showtext_opts(dpi = 300)

  # use different margins depending on if subtitle or title is passed
  theme_margins <- switch(
    margin_location,
    "title" = gg$theme(
      plot.title = gg$element_text(size = 14, margin = gg$margin(b = 0.05, unit = "in"))
    ),
    "subtitle" = gg$theme(
      plot.title = gg$element_text(size = 14, margin = gg$margin(b = 0.01, unit = "in")),
      plot.subtitle = gg$element_text(margin = gg$margin(b = 0.05, unit = "in"))
    )
  )

  gghdx$theme_hdx() +
    gg$theme(
      axis.text.x = gg$element_text(vjust = 1),
      axis.title = gg$element_text(size = 12),
      axis.text = gg$element_text(size = 11),
      plot.caption = gg$element_text(size = 8, hjust = 1, margin = gg$margin(t = 0.05, unit = "in")),
      plot.caption.position = "plot",
      legend.text = gg$element_text(size = 9),
      legend.title = gg$element_text(size = 11),
      panel.background = gg$element_rect(fill = "white", linewidth = 0),
      plot.background = gg$element_rect(fill = "white", linewidth = 0),
    ) +
    theme_margins
}
