box::use(gg = ggplot2)

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
#'
#' @returns Filepath the plot is saved to.
iso3_ggsave <- function(p, iso3, fp = tempfile(fileext = ".png")) {
  gg$ggsave(
    filename = fp,
    plot = p,
    width = 5,
    height = 4,
    units = "in"
  )

  fp
}
