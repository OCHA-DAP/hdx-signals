box::use(base64enc)
box::use(httr2)
box::use(ggplot2)
box::use(purrr)
box::use(png)
box::use(grid)

# local module
box::use(./base_api)
box::use(../../utils/gmas_test_run)

#' Save and encode image
#'
#' Saves image to temporary filepath and then encodes this in base 64. Mailchimp
#' requires file uploads in base 64, so is a way to convert any image on the system
#' into the necessary image.
#'
#' @params fp File path to the image
#'
#' @returns Raw encoded string of the image
encode_image <- function(fp) {
  # read image binary and encode
  image_binary <- readBin(
    con = fp,
    what = "raw",
    n = file.info(fp)$size
  )

  base64enc$base64encode(image_binary)
}

#' Upload image to Mailchimp
#'
#' Upload image to Mailchimp. Since Mailchimp requires that images within mails are
#' provided as links to the Mailchimp storage of the image itself, this is done
#' by encoding it and uploading it to Mailchimp using
#' the API. This function then returns the URL that can be used in the email
#' to link in the image using href.
#'
#' @param fp File path to the image
#' @param name Name of the object to be passed into the Mailchimp system.
#'
#' @returns URL string to reference object on Mailchimp servers
#'
#' @export
mc_upload_image <- function(fp, name) {
  encoded_image <- encode_image(fp)

  # upload image to Mailchimp
  req <- base_api$mc_api(lists_api = FALSE) |>
    httr2$req_url_path_append(
      "file-manager",
      "files"
    ) |>
    httr2$req_body_json(
      data = list(
        file_data = encoded_image,
        name = name,
        folder_id = 9 # HDX Signals file folder on Mailchimp
      )
    )

  if (gmas_test_run$gmas_test_run()) {
    # print out the image and return the dry run
    mc_test_image_view(fp)
    "test-image-url"
  } else {
    # upload the image and extract URL
    req |>
      httr2$req_perform() |>
      httr2$resp_body_json() |>
      purrr$pluck("full_size_url")
  }
}

#' Upload plot to Mailchimp
#'
#' Saves plots out with `ggplot2::ggsave()`, and then uploads them to mailchimp
#' using `mc_upload_image()`.
#'
#' @param plot `ggplot2::gplot` object
#' @param name Name of the object to be passed into the Mailchimp system.
#' @param ... Additional arguments passed to `ggplot2::ggsave()`
#'
#' @returns URL string to reference object on Mailchimp servers
#'
#' @examples
#' library(ggplot2)
#'
#'
#' p <- ggplot(mtcars) + geom_point(aes(x = mpg, y = wt))
#' mc_upload_plot(p, "test.jpg")
#'
#' @export
mc_upload_plot <- function(plot, name, ...) {
  tf <- tempfile(fileext = ".png")
  ggplot2$ggsave(
    filename = tf,
    plot = plot,
    ...
  )

  mc_upload_image(
    fp = tf,
    name = name
  )
}

#' View image for testing
#'
#' Used if `gmas_test_run()`, will read a saved out image and view directly the
#' png on the active graphics device for interactive testing.
mc_test_image_view <- function(fp) {
  # read and view new plot
  png$readPNG(fp) |>
    grid$grid.raster()

  "test-image-url"
}
