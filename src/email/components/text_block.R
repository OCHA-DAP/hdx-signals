box::use(glue)
box::use(uuid)

box::use(./missing[missing_text])

#' Add text block
#'
#' Add text block in the Mailchimp template style. Can pass in the body of
#' text, the header text, and the level of the header
#'
#' @param text Body of text to add. If missing, text div removed.
#' @param header Header text. If missing, header block removed.
#' @param header_level Level of the header, as a number. Corresponds to h1 to h6.
#' @param header_id ID for the header, which can be used in `href` blocks to
#'     place jump links in the email. If `NULL`, then random UUID used since
#'     HTML requires unique IDs.
#'
#' @returns Full text block HTML
#'
#' @export
add_text <- function(text = "", header = "", header_level = 2, header_id = NULL) {
  if (is.null(header_id)) {
    header_id <- paste0("a", uuid$UUIDgenerate()) # must start with letter
  }
  if (missing_text(text) && missing_text(header)) {
    ""
  } else {
    glue$glue(
      # nolint start
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

                            {conditional_header(header, header_level, header_id)}

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
      # nolint end
    )
  }
}

#' Drop header block if doesn't exist
#'
#' For the header block, uses <a> and <h> blocks separately so that anchor blocks
#' work across a wider range of browsers.
conditional_header <- function(header, header_level, header_id) {
  if (missing_text(header)) {
    ""
  } else {
    glue$glue('<a name="{header_id}"></a><h{header_level} id="{header_id}">{header}</h{header_level}></h>')
  }
}

#' Drop text block if doesn't exist
conditional_text <- function(text) {
  if (missing_text(text)) {
    ""
  } else {
    glue$glue("<div>{text}</div>")
  }
}
