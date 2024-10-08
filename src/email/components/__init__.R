#' @export
box::use(
  ./banner_block[...],
  ./conditional_merge[...],
  ./create_template[...],
  ./email_body[...],
  ./further_info_block[...],
  ./image_block[...],
  ./intro_block[...],
  ./line_block[...],
  ./location_block[...],
  ./missing[...],
  ./summary_block[...],
  ./text_block[...]
)

if (is.null(box::name())) {
  box::use(./`__tests__`)
}
