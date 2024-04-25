box::use(gg = ggplot2)

box::use(./geom_adm0)
box::use(../plots/theme_signals)

#' Create base map for country
#'
#' Creates the base map for a country. Initializes `ggplot2::ggplot()`, adds the
#' country boundaries base layer, and uses `theme_signals()`.
#'
#' @param iso3 ISO3 code
#'
#' @returns ggplot object
#'
#' @export
gg_map <- function(iso3) {
  gg$ggplot() +
    geom_adm0$geom_adm0(iso3) +
    theme_signals$theme_signals()
}
