box::use(base64enc)
box::use(httr2)
box::use(ggplot2)
box::use(purrr)

# local module
box::use(./base_api)

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
  response <- base_api$mc_api(lists = FALSE) |>
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
    ) |>
    httr2$req_perform()

  # extract the URL from the response
  response |>
    httr2$resp_body_json() |>
    purrr$pluck("full_size_url")
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
