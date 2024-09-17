box::use(glue)

box::use(src/email/components/missing)

#' Add banner block
#'
#' Add banner block in the Mailchimp template style. `src` link to the banner
#' can be passed in as a parameter.
#'
#' @returns Full image block HTML
#'
#' @export
add_banner <- function(src) {
  if (missing$missing_text(src)) {
    ""
  } else {
    glue$glue(
      # nolint start
      '
<tr>
    <td style="background-color:#007ce0;padding-top:0;padding-bottom:0;padding-right:0;padding-left:0"
        class="mceBlockContainer" align="full" valign="top"><a href="https://data.humdata.org/" style="display:block"
            target="_blank" data-block-id="3"><span class="mceImageBorder"
                style="border:0;vertical-align:top;margin:0"><img width="660" height="auto"
                    style="width:660px;height:auto;max-width:660px !important;display:block" alt=""
                    src="{src}"
                    role="presentation" class="imageDropZone mceLogo" /></span></a></td>
</tr>
      '
      # nolint end
    )
  }
}
