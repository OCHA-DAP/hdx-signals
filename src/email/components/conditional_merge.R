box::use(glue)

box::use(../../utils/location_codes)

#' Wraps text in conditional merge text
#'
#' Wraps text in conditional merge text based on the ISO3 code passed in. This is
#' also wrapped in text so the conditional merge tags only appear if the campaign
#' is viewed in the email! Thus, the archive page can show all content. If
#' `use_conditions` is `FALSE`, then the text is just returned as is. This is
#' because archive links don't work.
#'
#' The logic of the merge statement below is designed around users being able to
#' sign up to an entire region or specific countries (for some countries).
#'
#' @param text Text to wrapped in conditional merge text
#' @param iso3 Location ISO3 code
#' @param use_conditions Whether or not to use the conditional merge text. If `FALSE`,
#'     `text` is returned unchanged.
#'
#' @returns Text wrapped in conditional merge if necessary
#'
#' @export
conditional_merge <- function(text, iso3, use_conditions) {
  if (use_conditions) {
    region <- location_codes$iso3_to_regions(iso3)
    location <- location_codes$iso3_to_names(iso3)
    glue$glue(
      "*|INTERESTED:{region}:All countries in the region|*",
      "*|INTERESTED:{region}:{location}|*",
      "*|ELSE:|*",
      "{text}",
      "*|END:INTERESTED|*",
      "*|END:INTERESTED|*",
      "*|INTERESTED:{region}:{location}|*",
      "{text}",
      "*|END:INTERESTED|*"
    )
  } else {
    text
  }
}
