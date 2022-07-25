#' Flag values in top perc
#' 
#' @param x Numeric vector
#' @param perc % to flag. Defaults to flagging values in the top 5% (above
#'     95% of values).
#' @param exclude_zero Exclude zero from quantile calculation. Defaults to TRUE.
flag_percent <- function(x, perc = 0.95, exclude_zero = TRUE) {
  lim <- flag_lim(x = x, perc = perc, exclude_zero = exclude_zero)
  x >= lim
}

#' Get limits for flagging
#'
#' @inheritParams flag_percent
flag_lim <- function(x, perc = 0.95, exclude_zero = TRUE) {
  if (exclude_zero) {
    x_check <- x[x != 0]
  } else {
    x_check <- x
  }
  
  quantile(x_check, probs = perc, na.rm = TRUE)
}


#' Get outliers from time series
#' 
#' Automatic detection using `tsoutliers()`.
tsoutliers_clean <- function(x, row_numbers) {
  outliers <- tsoutliers(x)
  indices <- outliers[["index"]]
  if (length(indices) == 0) {
    rep(FALSE, length(x))
  } else {
    row_numbers %in% indices
  }
}