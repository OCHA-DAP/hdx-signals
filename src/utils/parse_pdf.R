box::use(pdftools)

#' Parse PDF text from URL
#'
#' Extracts all text from PDF. No other analysis is done, but is easy to pass
#' straight for summarization in this format
#'
#' @param url URL to PDF
#'
#' @returns Single string of all text in the PDF
#'
#' @export
parse_pdf <- function(url) {
  pdftools$pdf_text(url) |>
    paste(
      sep = "\n",
      collapse = "\n"
    )
}
