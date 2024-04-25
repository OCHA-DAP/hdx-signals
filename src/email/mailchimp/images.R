box::use(base64enc)
box::use(httr2)
box::use(ggplot2)
box::use(purrr)
box::use(png)
box::use(grid)

# local module
box::use(./base_api)
box::use(../../utils/gmas_test_run)
box::use(./folders)

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
#' @param folder Name of the file folder on Mailchimp.
#' @param preview Whether or not to preview the saved plot when `gmas_test_run()`
#'     is `TRUE`.
#'
#' @returns URL string to reference object on Mailchimp servers
#'
#' @export
mc_upload_image <- function(fp, name, folder, preview = FALSE) {
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
        folder_id = folders$mc_file_folder_id(folder)
      )
    )

  if (gmas_test_run$gmas_test_run()) {
    # print out the image and return the dry run
    data.frame(
      id = "test-image-id",
      url = mc_test_image_view(fp, preview)
    )
  } else {
    # upload the image and extract URL
    resp <- req |>
      httr2$req_perform() |>
      httr2$resp_body_json()

    data.frame(
      id = as.character(resp$id),
      url = resp$full_size_url
    )
  }
}

#' Upload plot to Mailchimp
#'
#' Saves plots out with `ggplot2::ggsave()`, and then uploads them to mailchimp
#' using `mc_upload_image()`.
#'
#' @param plot `ggplot2::gplot` object
#' @param name Name of the object to be passed into the Mailchimp system.
#' @param folder Name of the file folder on Mailchimp.
#' @param id File ID on Mailchimp. If `NULL`, creates a new image. If not `NULL`,
#'     updates the existing image with that id.
#' @param preview Whether or not to preview the saved plot when `gmas_test_run()`
#'     is `TRUE`.
#' @param height Height of the plot (in inches)
#' @param width Width of the plot (in inches)
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
mc_upload_plot <- function(plot, name, folder, preview = FALSE, height, width, ...) {
  tf <- tempfile(fileext = ".png")
  ggplot2$ggsave(
    filename = tf,
    plot = plot,
    height = height,
    width = width,
    units = "in",
    dpi = 300,
    ...
  )

  mc_upload_image(
    fp = tf,
    name = name,
    folder = folder,
    preview = preview
  )
}

#' Save and encode image
#'
#' Saves image to temporary filepath and then encodes this in base 64. Mailchimp
#' requires file uploads in base 64, so is a way to convert any image on the system
#' into the necessary format
#'
#' @param fp File path to the image
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

#' View image for testing
#'
#' Used if `gmas_test_run()`, will read a saved out image and view directly the
#' png on the active graphics device for interactive testing.
mc_test_image_view <- function(fp, preview = FALSE) {
  # read and view new plot
  if (preview) {
    png$readPNG(fp) |>
      grid$grid.raster()
  }

  paste0("file://", fp)
}
