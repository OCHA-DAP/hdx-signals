box::use(box.linters)

#' Useless code that simply prints the default box linters, used so that the
#' `{box.linters}` package is captured in the renv snapshot without requiring
#' a DESCRIPTION file.
#'
#' @export
box.linters$box_default_linters
