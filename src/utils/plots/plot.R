box::use(gg = ggplot2)

box::use(../plots/plot_displacement[plot_displacement])
box::use(../plots/plot_food_insecurity[plot_food_insecurity])
box::use(../plots/plot_cholera[plot_cholera])

#' Generate plot for a specific alert and country
#'
#' `plot_general()` generates a specific plot based on `flag_source` and `iso3`.
#' The plotting functions are matched based on `flag_source` and then run with
#' `iso3` and `country`. Thus, all `plot_...()` functions must have the same
#' API of requiring only the `iso3`, which is used to filter the wrangled data.
#'
#' Current supported functions are `plot_displacement()`, `plot_food_insecurity()`, and
#' `plot_cholera()`.
#'
#' @param flag_type Type of the flag/alert, which should match the second
#'     portion of supported plotting functions (e.g. `displacement` or `food_insecurity`).
#' @param iso3 ISO3 code.
#'
#' @returns Relevant plot.
#'
#' @examples
#' # easily generate plots, jsut pass in the type and iso3
#' plot_general("food_insecurity", "AFG")
#' plot_general("cholera", "NGA")
#'
#' @export
plot_general <- function(flag_type, iso3) {
  # match.fun was not working with box, so just manually creating list of functions
  fun_list <- list(
    food_insecurity = plot_food_insecurity,
    displacement = plot_displacement,
    cholera = plot_cholera
  )

  # create the plot and make a few edits
  fun_list[[flag_type]](iso3 = iso3) +
    gg$theme(
      plot.title = gg$element_text(size = 14),
      axis.title = gg$element_text(size = 10),
      axis.text = gg$element_text(size = 10)
    )
}
