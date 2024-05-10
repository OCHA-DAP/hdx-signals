box::use(dplyr)

#' Format date in correct form
#'
#' Formats date to have the 8 August 2023 format: no leading zero on the day,
#' full month name, and full year. Also trims the whitespace around the date
#'
#' @param date Date to format.
#'
#' @export
format_date <- function(date) {
  trimws(format(date, format = "%e %B %Y"))
}

#' Format number as key figures
#'
#' TODO: Move to `{gghdx}` when PR has been merged.
#'
#' Formats numeric vector of key figures in the Centre style, which abbreviates
#' numbers 1,000 and above to X.YK, 10,000 and above to XYK, 100,000 and above
#' to XYZK, and the same for 1,000,000 and above, replacing the K with an M, and
#' the same for B.
#'
#' Deals with negative values in case those ever need to be formatted in similar
#' manners. Also ensures that rounding is performed so numbers look correct.
#'
#' @param x Numeric vector to format
#' @param additional_prefix Additional prefix to add to string, that will come
#'     between `sign_prefix` and the number. For example, `"$"` could produce a
#'     return value of `-$1.1K`.
#'
#' @returns Character vector of formatted strings
#'
#' @export
format_key_figures <- function(x, additional_prefix = "") {
  sign_x <- sign(x)
  sign_prefix <- ifelse(sign_x == -1, "-", "")
  abs_x <- abs(x)

  dplyr$case_when(
    abs_x < 1e3 ~ paste0(sign_prefix, additional_prefix, round(abs_x, digits = 0)),
    abs_x < 1e4 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e3, digits = 1), "K"),
    abs_x < 1e6 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e3, digits = 0), "K"),
    abs_x < 1e7 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e6, digits = 1), "M"),
    abs_x < 1e9 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e6, digits = 0), "M"),
    abs_x < 1e10 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e9, digits = 1), "B"),
    abs_x < 1e12 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e9, digits = 0), "B"),
    abs_x < 1e13 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e12, digits = 1), "T"),
    abs_x >= 1e13 ~ paste0(sign_prefix, additional_prefix, round(abs_x / 1e12, digits = 0), "T")
  )
}
