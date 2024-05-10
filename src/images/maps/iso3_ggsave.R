box::use(gg = ggplot2)
box::use(dplyr)
box::use(rlang[`!!`])
box::use(knitr)
box::use(magick) # silent requirement for knitr::plot_crop()

box::use(cs = ../../utils/cloud_storage)

#' Saves plot to filepath for ISO3
#'
#' Saves plot `p` to `fp` for an `iso3` code. Used to set custom save parameters
#' to adjust the width/height and other parameters when saving the plot. By default
#' saves to a `tempfile()`, but can be saved to any filepath provided.
#'
#' @param p Plot
#' @param iso3 ISO3 code
#' @param fp Filepath to save to, should include `.png` extension.
#'     Defaults to `tempfile()`.
#' @param crop Whether or not to crop the plot with `knitr::plot_crop()`.
#'
#' @returns Filepath the plot is saved to.
#'
#' @export
iso3_ggsave <- function(p, iso3, fp = tempfile(fileext = ".png"), crop = TRUE) {
  df_wh <- dplyr$filter(df_width_height, iso3 == !!iso3)
  gg$ggsave(
    filename = fp,
    plot = p,
    width = df_wh$width,
    height = df_wh$height,
    units = "in"
  )

  if (crop) {
    knitr$plot_crop(fp)
  }

  fp
}

df_width_height <- cs$read_az_file("input/iso3_width_height.parquet")
