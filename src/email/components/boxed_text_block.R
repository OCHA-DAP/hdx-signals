box::use(glue)

box::use(./missing[missing_text])

#' Add boxed test block
#'
#' Add boxed text block in the Mailchimp template style. Only accepts a body
#' of text.
#'
#' @param text Body of text to add. If missing, text div removed.
#'
#' @returns Full text block HTML
#'
#' @export
add_boxed_text <- function(text) {
  if (missing_text(text)) {
    ""
  } else {
    glue$glue(
      '<table border="0" cellpadding="0" cellspacing="0" width="100%" class="mcnBoxedTextBlock" style="min-width:100%;">
    <!--[if gte mso 9]>
	<table align="center" border="0" cellspacing="0" cellpadding="0" width="100%">
	<![endif]-->
	<tbody class="mcnBoxedTextBlockOuter">
        <tr>
            <td valign="top" class="mcnBoxedTextBlockInner">

				<!--[if gte mso 9]>
				<td align="center" valign="top" ">
				<![endif]-->
                <table align="left" border="0" cellpadding="0" cellspacing="0" width="100%" style="min-width:100%;" class="mcnBoxedTextContentContainer">
                    <tbody><tr>

                        <td style="padding-top:9px; padding-left:18px; padding-bottom:9px; padding-right:18px;">

                            <table border="0" cellspacing="0" class="mcnTextContentContainer" width="100%" style="min-width: 100% !important;background-color: #EEEEEE;">
                                <tbody><tr>
                                    <td valign="top" class="mcnTextContent" style="padding: 18px;color: #000000;font-family: &quot;Source Sans Pro&quot;, &quot;Helvetica Neue&quot;, Helvetica, Arial, sans-serif;font-size: 14px;font-weight: normal;text-align: center;">
                                      {text}
                                    </td>
                                </tr>
                            </tbody></table>
                        </td>
                    </tr>
                </tbody></table>
				<!--[if gte mso 9]>
				</td>
				<![endif]-->

				<!--[if gte mso 9]>
                </tr>
                </table>
				<![endif]-->
            </td>
        </tr>
    </tbody>
</table>'
    )
  }
}
