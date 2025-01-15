box::use(
  dplyr,
  tidyr,
  scales,
  gg = ggplot2,
  gghdx,
  lubridate,
  readr,
  ggrepel,
  utils,
  glue,
  grid,
  gridExtra,
)

box::use(
  src/images/plots/theme_signals,
  src/images/plots/caption,
  src/images/plots/breaks_date,
  src/images/create_images
)

#' Plot ACAPS INFORM index
#'
#' Creates time series of INFORM index
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Wrangled data frame
#' @param df_raw
#' @param preview Whether or not to preview the plots
#'
#' @export
table <- function(df_alerts, df_wrangled, df_raw, preview = FALSE) {
  # add title for use in the plot
  df_plot <- df_alerts |>
    dplyr$mutate(
      title = "Drivers of the ongoing crisis"
    )

  create_images$create_images(
    df_alerts = df_plot,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    image_fn = inform_table,
    image_use = "table"
  )
}

#' Plot ACAPS INFORM index data and sub-indicators.
#'
#' @param df_wrangled Wrangled data frame for plotting.
#' @param df_raw Raw data frame for plotting, not used to plot displacement time
#'     series
#' @param title Plot title.
#' @param date Date of the alert. Not used in the plot.
#'
#' @returns Plot of cholera for that wrangled data
inform_table <- function(df_wrangled, df_raw, title, date) {
  df_wrangled<- df_wrangled[order(df_wrangled$date, decreasing=TRUE),]
  # Define a function to wrap text
  wrap_text <- function(x, width = 15) {
    sapply(x, function(y) paste(strwrap(y, width = width), collapse = "\n"))
  }

  df_table <- df_wrangled[!duplicated(df_wrangled$crisis_id), c("crisis_name", "drivers")]
  df_table <- as.data.frame(lapply(df_table, wrap_text, width = 40))  # Wrap text to fit cell width
  colnames(df_table) <- c("Crisis name", "Drivers")
  table <- gridExtra$tableGrob(df_table[, c("Crisis name", "Drivers")], rows = NULL)
  # Create a title as a textGrob
  title <- grid$textGrob("Drivers of the ongoing crisis",
                    gp = grid$gpar(fontsize = 12, fontface = "bold"), just="left", x=grid$unit(0, "npc"))

  # Arrange the title and table in a single layout
  table <- gridExtra$grid.arrange(title, table,
                              ncol = 1,  # Arrange in one column
                              heights = grid$unit(c(1, 6), c("lines", "null")))
  table
}
