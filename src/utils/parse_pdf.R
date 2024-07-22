box::use(pdftools)

#' Parse PDF text from URL
#'
#' Extracts all text from PDF. No other analysis is done, but is easy to pass
#' straight for summarization in this format. In case of URL blocking or other
#' parsing errors, do a simple tryCatch to ensure NA value is returned and
#' full errors aren't generated simply because PDF is unscrapeable.
#'
#' @param url URL to PDF
#'
#' @returns Single string of all text in the PDF
#'
#' @export
parse_pdf <- function(url) {
  tryCatch(
    {
      suppressMessages(
        pdftools$pdf_text(url)
      ) |>
        paste(
          sep = "\n",
          collapse = "\n"
        )
    },
    error = \(e) NA_character_
  )
}
