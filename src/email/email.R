# utility functions for the emails

# creates a custom add image by using code that makes the image responsive
# even in Windows Outlook
# based on here: https://stackoverflow.com/questions/2426072/is-there-an-equivalent-of-css-max-width-that-works-in-html-emails
add_image_custom <- function(
    file,
    alt = "",
    width = 520,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
) {
  # get default blastula image HTML
  html <- blastula::add_image(
    file = file,
    alt = alt,
    width = width,
    align = align,
    float = float
  )

  custom_html(
    html = html,
    width = width
  )
}

# use add_image to save out plots and add them back to the file
# use custom width
add_ggplot_custom <- function(
    plot_object,
    width = 5,
    height = 5,
    html_width = 1000,
    alt = NULL,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
) {
  html <- blastula::add_ggplot(
    plot_object = plot_object,
    width = width,
    height = height,
    alt = alt,
    align = align,
    float = float
  )

  custom_html(
    html = html,
    width = html_width
  )
}

custom_html <- function(html, width) {
  img_html <- stringr::str_extract(
    html,
    "(<img src.*px;\"/>)",
    group = 1
  )
  img_html_styled <- stringr::str_replace(
    img_html,
    "(?<=style=\")(.*)(?=\"/>)",
    "display:block;width:100%"
  )

  # create the new custom table for the HTML
  cat(
    paste0(
      '<table border="0" cellspacing="0" width="100%"><tr><td></td><td width="',
      width,
      '">',
      img_html_styled,
      "</td><td></td></tr></table>"
    )
  )
}
