#' @export
box::use(
  src/email/mailchimp/audience[...],
  src/email/mailchimp/base_api[...],
  src/email/mailchimp/campaigns[...],
  src/email/mailchimp/custom_segmentation[...],
  src/email/mailchimp/delete[...],
  src/email/mailchimp/folders[...],
  src/email/mailchimp/images[...],
  src/email/mailchimp/segments[...],
  src/email/mailchimp/tags[...],
  src/email/mailchimp/templates[...]
)

if (is.null(box::name())) {
  box::use(src/email/mailchimp/`__tests__`)
}
