box::use(
  glue,
  uuid
)

box::use(src/email/components/missing)

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
add_text <- function(
    text = "",
    header = "",
    header_level = 2,
    header_id = NULL,
    pre_header_text = "",
    background_color = "#ffffff") {
  if (is.null(header_id)) {
    header_id <- paste0("a", uuid$UUIDgenerate()) # must start with letter
  }

  text_missing <- missing$missing_text(text)
  header_missing <- missing$missing_text(header)
  pre_header_text_missing <- missing$missing_text(pre_header_text)

  if (text_missing && header_missing && pre_header_text_missing) {
    ""
  } else {
    # create all pieces of text to inject into the body
    preheader <- conditional_text(
      text = pre_header_text,
      text_missing = pre_header_text_missing
    )

    preheader_sep <- conditional_sep(
      a_missing = pre_header_text_missing,
      b_missing = header_missing
    )

    header <- conditional_header(
      header = header,
      header_level = header_level,
      header_id = header_id,
      header_missing = header_missing
    )

    header_sep <- conditional_sep(
      a_missing = header_missing,
      b_missing = text_missing
    )

    text_body <- conditional_text(
      text = text,
      text_missing = text_missing
    )

    glue$glue(
      # nolint start
      '
<tr>
    <td style="padding-top:0;padding-bottom:0;padding-right:0;padding-left:0" valign="top">
        <table width="100%" style="border:0;background-color:{background_color};border-collapse:separate">
            <tbody>
                <tr>
                    <td style="padding-left:24px;padding-right:24px;padding-top:12px;padding-bottom:12px"
                        class="mceTextBlockContainer">
                        <div class="mceText" style="width:100%">
                            {preheader}
                            {preheader_sep}
                            {header}
                            {header_sep}
                            {text_body}
                        </div>
                    </td>
                </tr>
            </tbody>
        </table>
    </td>
</tr>
      '
      # nolint end
    )
  }
}

#' Drop header block if doesn't exist
#'
#' For the header block, uses <a> and <h> blocks separately so that anchor blocks
#' work across a wider range of browsers.
conditional_header <- function(header, header_level, header_id, header_missing) {
  if (header_missing) {
    ""
  } else {
    glue$glue(
      '
      <a name="{header_id}"></a><h{header_level} id="{header_id}" ',
      'class="last-child">{header}</h{header_level}>'
    )
  }
}

#' Add empty paragraph with line break
#'
#' Adds empty paragraph with line break if both a and b are present (not missing).
conditional_sep <- function(a_missing, b_missing) {
  if (!a_missing && !b_missing) {
    "<p><br/></p>"
  } else {
    ""
  }
}

#' Drop text block if doesn't exist
conditional_text <- function(text, text_missing) {
  if (text_missing) {
    ""
  } else {
    glue$glue("<p>{text}</p>")
  }
}
