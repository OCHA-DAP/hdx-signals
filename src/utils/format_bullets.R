box::use(stringr)

#' Put each bullet of AI generated text on its own line
#'
#' HTML collapses plain newlines, so a "-" bullet list in AI generated summary
#' text runs together as one paragraph in the email. This replaces the break
#' before each bullet with `<br>` so every bullet starts on a new line. A bullet
#' is a `-`, `*` or bullet character that either starts a line or follows the
#' end of a sentence.
#'
#' @param text Character vector of summary text
#'
#' @returns Character vector of text, with a `<br>` before each bullet
#'
#' @examples
#' format_bullets("Actions:\n- Scale up aid.\n- Fund nutrition.")
#' # "Actions:<br>- Scale up aid.<br>- Fund nutrition."
#'
#' @export
format_bullets <- function(text) {
  text |>
    stringr$str_replace_all("\\s*\\n\\s*[-*\\u2022]\\s+", "<br>- ") |>
    stringr$str_replace_all("(?<=[.:;!?])\\s+[-*\\u2022]\\s+(?=\\S)", "<br>- ")
}
