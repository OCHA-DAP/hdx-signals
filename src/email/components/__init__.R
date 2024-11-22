#' @export
box::use(
  src/email/components/banner_block[...],
  src/email/components/conditional_merge[...],
  src/email/components/create_template[...],
  src/email/components/email_body[...],
  src/email/components/further_info_block[...],
  src/email/components/image_block[...],
  src/email/components/intro_block[...],
  src/email/components/line_block[...],
  src/email/components/location_block[...],
  src/email/components/missing[...],
  src/email/components/summary_block[...],
  src/email/components/text_block[...]
)

if (is.null(box::name())) {
  box::use(src/email/components/`__tests__`)
}
