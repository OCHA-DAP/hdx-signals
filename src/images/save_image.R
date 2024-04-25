box::use(dplyr)
box::use(rlang[`!!`])

box::use(../email/mailchimp/folders)
box::use(cs = ../utils/cloud_storage)
box::use(../email/mailchimp/images)

#' Save image produced for HDX Signals
#'
#' Saves image produced for HDX signals. Takes in a plot object `p`, gives it a
#' name based on the country, indicator, and date, then uploads it. The returned
#' data frame of ID and URL has names based on `image_use`, whether they are the
#' initial `plot`, the `map`, or the `plot2` image for the campaign.
#'
#' @param p Plot object
#' @param iso3 ISO3 code
#' @param indicator_id Indicator ID
#' @param date Date of the alert
#' @param width Width of the image (in inches)
#' @param height Height of the image (in inches)
#' @param ... Additional arguments passed to `ggplot2::ggsave()`
#'
#' @export
save_image <- function(p, iso3, indicator_id, date, width, height, ...) {
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
    preview = FALSE,
    width = width,
    height = height,
    ...
  )
}

df_folders <- cs$read_az_file("input/indicator_mapping.parquet")
