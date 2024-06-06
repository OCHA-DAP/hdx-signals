box::use(glue)

#' Add line divider block
#'
#' Add line divider block in the Mailchimp template style. No parameters to pass
#' in, just returns the block when called.
#'
#' @returns Full line divider block HTML
#'
#' @export
add_line <- function() {
  # nolint start
  '
<tr>
    <td style="background-color:#ffffff;padding-top:20px;padding-bottom:20px;padding-right:24px;padding-left:24px"
        class="mceBlockContainer" valign="top">
        <table align="center" border="0" cellpadding="0" cellspacing="0" width="100%"
            style="background-color:#ffffff;width:100%" role="presentation" class="mceDividerContainer"
            data-block-id="84">
            <tbody>
                <tr>
                    <td style="min-width:100%;border-top-width:2px;border-top-style:solid;border-top-color:#d6d6d6"
                        class="mceDividerBlock" valign="top"></td>
                </tr>
            </tbody>
        </table>
    </td>
</tr>
  '
  # nolint end
}
