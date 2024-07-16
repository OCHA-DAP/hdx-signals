box::use(glue)

box::use(./missing[missing_text])

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
add_image <- function(src, alt = "", caption = "") {
  if (missing_text(src)) {
    ""
  } else {
    glue$glue(
      # nolint start
      '
<tr>
    <td style="padding-top:10px;padding-bottom:10px;padding-right:0;padding-left:0" class="mceBlockContainer" align="full" valign="top">
        <span class="mceImageBorder" style="border:0;vertical-align:top;margin:0">
            <img
                width="660" height="auto"
                style="width:660px;height:auto;max-width:1800px !important;display:block"
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
