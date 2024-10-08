box::use(
  src/email/components/text_block,
  src/email/components/missing[missing_text]
)

#' Further info block
#'
#' Creates further info block using `text` and generic headers and text.
#'
#' @param text Summary text
#'
#' @export
add_further_info <- function(text) {
  if (missing_text(text)) {
    ""
  } else {
    text_block$add_text(
      header = "Further information",
      text = text,
      header_level = 3
    )
  }
}
