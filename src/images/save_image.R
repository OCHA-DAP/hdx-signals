box::use(dplyr)
box::use(rlang[`!!`])

box::use(src/email/mailchimp/folders)
box::use(src/email/mailchimp/images)

box::use(cs = src/utils/cloud_storage)

#' Save image produced for HDX Signals
#'
#' Saves image produced for HDX signals. Takes in a plot object `p`, gives it a
#' name based on the location, indicator, and date, then uploads it. The returned
#' data frame of ID and URL has names based on `image_use`, whether they are the
#' initial `plot`, the `map`, or the `plot2` image for the campaign.
#'
#' @param p Plot object
#' @param iso3 ISO3 code
#' @param indicator_id Indicator ID
#' @param date Date of the alert
#' @param width Width of the image (in inches)
#' @param height Height of the image (in inches)
#' @param crop Whether or not to run `knitr::plot_crop()` is run on the image
#'     to crop white space around the image automatically.
#' @param ... Additional arguments passed to `ggplot2::ggsave()`
#'
#' @export
save_image <- function(p, iso3, indicator_id, date, width, height, crop = FALSE, ...) {
  name <- paste(tolower(iso3), indicator_id, format(date, "%Y_%m_%b.png"), sep = "_")

  # get the folder name for the indicator ID
  folder <- df_folders |>
    dplyr$filter(
      indicator_id == !!indicator_id
    ) |>
    dplyr$pull(
      mc_folder
    )

  images$mc_upload_plot(
    plot = p,
    name = name,
    folder = folder,
    width = width,
    height = height,
    crop = crop,
    preview = FALSE,
    ...
  )
}

#' Save map produced by HDX Signals
#'
#' Just a wrapper around `save_image()` that automatically pulls in the width
#' and height for the ISO3 code.
#'
#' @param p Plot object
#' @param iso3 ISO3 code
#' @param indicator_id Indicator ID
#' @param date Date of the alert
#' @param crop Whether or not to run `knitr::plot_crop()` is run on the image
#'     to crop white space around the image automatically.
#'
#' @export
save_map <- function(p, iso3, indicator_id, date, crop = FALSE) {

  # get the width and height
  df_ms <- dplyr$filter(df_map_settings, iso3 == !!iso3)

  save_image(
    p = p,
    iso3 = iso3,
    indicator_id = indicator_id,
    date = date,
    width = df_ms$width,
    height = df_ms$height,
    crop = crop
  )
}

df_folders <- cs$read_az_file("input/indicator_mapping.parquet")
df_map_settings <- cs$read_az_file("input/iso3_map_settings.json")
