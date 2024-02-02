box::use(glue)

#' Add text block
#'
#' Add text block in the Mailchimp template style. Can pass in the body of
#' text, the header text, and the level of the header
#'
#' @param text Body of text to add. If missing, text div removed.
#' @param header Header text. If missing, header block removed.
#' @param header_level Level of the header, as a number. Corresponds to h1 to h6.
#'
#' @returns Full text block HTML
#'
#' @export
add_text <- function(text, header = "", header_level = 2) {
  glue$glue(
    '<table border="0" cellpadding="0" cellspacing="0" width="100%" class="mcnTextBlock" style="min-width:100%;">
    <tbody class="mcnTextBlockOuter">
        <tr>
            <td valign="top" class="mcnTextBlockInner" style="padding-top:9px;">
              	<!--[if mso]>
				<table align="left" border="0" cellspacing="0" cellpadding="0" width="100%" style="width:100%;">
				<tr>
				<![endif]-->

				<!--[if mso]>
				<td valign="top" width="600" style="width:600px;">
				<![endif]-->
                <table align="left" border="0" cellpadding="0" cellspacing="0" style="max-width:100%; min-width:100%;" width="100%" class="mcnTextContentContainer">
                    <tbody><tr>

                        <td valign="top" class="mcnTextContent" style="padding: 0px 18px 9px; font-size: 16px; line-height: 150%;">

                            {conditional_header(header, header_level)}

                            {conditional_text(text)}

                        </td>
                    </tr>
                </tbody></table>
				<!--[if mso]>
				</td>
				<![endif]-->

				<!--[if mso]>
				</tr>
				</table>
				<![endif]-->
            </td>
        </tr>
    </tbody>
</table>'
  )
}

#' Drop header block if doesn't exist
conditional_header <- function(header, header_level) {
  if (is.null(header) | is.na(header) | header == "") {
    ""
  } else {
    glue$glue('"<h{header_level} class="null">{header}</h{header_level}>"')
  }
}

#' Drop text block if doesn't exist
conditional_text <- function(text) {
  if (is.null(text) | is.na(text) | text == "") {
    ""
  } else {
    glue$glue("<div>{text}</div>")
  }
}
