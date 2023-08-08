#' Format date in correct form
format_date <- function(date) {
  gsub("^0", "", format(date, format = "%d %B %Y"))
}
