box::use(glue)

box::use(src/email/components/missing[missing_text])

#' Add image block
#'
#' Add image block in the Mailchimp template style. `src` link, `alt` text,
#' and captions can be passed in as parameters. If `src` is missing, returns
#' empty text.
#'
#' @param src Source image URL
#' @param alt Alternative image text
#' @param caption Caption text
#'
#' @returns Full image block HTML
#'
#' @export
add_image <- function(src, alt = "", caption = "", img_width="660px") {
  if (missing_text(src)) {
    ""
  } else {
    glue$glue(
      # nolint start
      '
<tr>
    <td style="padding-top:10px;padding-bottom:10px;text-align:center;" class="mceBlockContainer" valign="top">
        <span class="mceImageBorder" style="border:0;vertical-align:top;margin:0;">
            <img
                style="width:{img_width};height:auto;max-width:1800px !important;display:block"
                alt="{alt}"
                src="{src}"
                class="mceImage" />
                <figcaption>{caption}</figcaption>
        </span>
    </td>
</tr>
      '
      # nolint end
    )
  }
}
