box::use(glue)

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
  if (is.null(src) | is.na(src) | src == "") {
    ""
  } else {
    glue$glue(
      '<table border="0" cellpadding="0" cellspacing="0" width="100%" class="mcnImageBlock" style="min-width:100%;">
    <tbody class="mcnImageBlockOuter">
            <tr>
                <td valign="top" style="padding:0px" class="mcnImageBlockInner">
                    <table align="left" width="100%" border="0" cellpadding="0" cellspacing="0" class="mcnImageContentContainer" style="min-width:100%;">
                        <tbody><tr>
                            <td class="mcnImageContent" valign="top" style="padding-right: 0px; padding-left: 0px; padding-top: 0; padding-bottom: 0; text-align:center;">
                                <figure>
                                      <img align="center" alt="{alt}" src="{src}" width="600" style="max-width:1642px; padding-bottom: 0; display: inline !important; vertical-align: bottom;" class="mcnImage">
                                      <figcaption>{caption}</figcaption>
                                </figure>
                            </td>
                        </tr>
                    </tbody></table>
                </td>
            </tr>
    </tbody>
</table>'
    )
  }
}
