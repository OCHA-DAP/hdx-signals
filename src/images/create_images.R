box::use(dplyr)
box::use(janitor)
box::use(purrr)
box::use(rlang[`!!`])
box::use(rlang)

box::use(./save_image)

#' Overall workhorse to generate images
#'
#' Generates images from a data frame of alerts and a wrangled data frame. Requires
#' an image generation function to be passed in.
#'
#' @param df_alerts Data frame of alerts, with an additional `title` column added
#'     in that will be used to title the image.
#' @param df_wrangled Data frame of wrangled data for use in plotting.
#' @param df_raw Data frame of raw data for use in plotting.
#' @param image_fn Image generating function. Should accept only 3 arguments:
#'     `df_wrangled` and `df_raw` for the data frames to be used for plotting, and
#'     `title` to be used for the plot title.
#' @param image_use Where the image will be used, either as the default `plot`,
#'     the `map`, or `plot2` in the campaign.
#' @param height Height of the image in inches.
#' @param width Width of the image in inches.
#' @param use_map_settings Whether or not to use the map settings when saving. If
#'     `TRUE`, uses map dimensions in `input/iso3_map_settings.json` instead
#'     of the `width` and `height` arguments.
#' @param crop Whether or not to run `knitr::plot_crop()` is run on the image
#'     to crop white space around the image automatically.
#'
#' @export
create_images <- function(
    df_alerts,
    df_wrangled,
    df_raw,
    image_fn,
    image_use = c("plot", "map", "plot2"),
    width = 6,
    height = 4,
    use_map_settings = FALSE,
    crop = FALSE) {
  validate_images_alerts(df_alerts)
  validate_filter_df(df_wrangled)
  image_use <- rlang$arg_match(image_use)
  df_images <- purrr$pmap(
    .l = df_alerts,
    .f = \(iso3, date, title, indicator_id, ...) {
      create_image_poss(
        iso3 = iso3,
        date = date,
        title = title,
        indicator_id = indicator_id,
        df_wrangled = df_wrangled,
        df_raw = df_raw,
        image_fn = image_fn,
        width = width,
        heigh = height,
        use_map_settings = use_map_settings,
        crop = crop
      )
    }
  ) |>
    purrr$list_rbind()

  # rename the output data frame to match the use of the image
  df_images <- dplyr$bind_cols(dplyr$select(df_alerts, title), df_images)
  names(df_images) <- paste(image_use, names(df_images), sep = "_")
  df_images
}

#' Create single image for iso3, date, and indicator_id
#'
#' needs to be called from environment where `df_wrangled` and `image_fn` exists.
#' Creates the plot and saves it to Mailchimp. Used within `create_images()` to
#' safely create plot and catch errors so Mailchimp can be scrubbed of all content
#' created even if errors happen in the environment.
create_image <- function(
    iso3,
    date,
    title,
    indicator_id,
    df_wrangled,
    df_raw,
    image_fn,
    width,
    height,
    use_map_settings,
    crop) {
  df_wrangled <- filter_plot_df(
    iso3 = iso3,
    date = date,
    df = df_wrangled
  )

  p <- image_fn(df_wrangled, df_raw, title, date)
  if (use_map_settings) {
    save_image$save_map(
      p = p,
      iso3 = iso3,
      indicator_id = indicator_id,
      date = date,
      crop = crop
    )
  } else {
    save_image$save_image(
      p = p,
      iso3 = iso3,
      indicator_id = indicator_id,
      date = date,
      width = width,
      height = height,
      crop = crop
    )
  }
}

#' Generate errors upon image creation but still create data frame
create_image_poss <- purrr$possibly(create_image, data.frame(id = "ERROR", url = "ERROR"))

#' Filters data frame for image generation
#'
#' Filters the data frame image generation. Takes in the `iso3` code
#' and `date` of an alert, and filters the wrangled data frame to that country.
#' If the `date` is older than 90 days, then the wrangled data frame is filtered
#' to only have data up to the `date`.
filter_plot_df <- function(iso3, date, df) {
  df_iso3 <- dplyr$filter(df, iso3 == !!iso3)
  if (Sys.Date() - date > 90) {
    df_iso3 <- dplyr$filter(df_iso3, date <= !!date)
  }
  df_iso3
}


#' Validates that the correct alerts data frame is passed
#'
#' Checks that columns are present and have correct typing. Particularly checks
#' that a temporary `title` column has been added to the base alerts data frame,
#' which will be used as the plot title.
#'
#' @param df Data frame of alerts to validate
validate_images_alerts <- function(df) {
  if (!all(c("iso3", "date", "title", "indicator_id") %in% names(df))) {
    stop(
      "Alerts data frame does not have all necessary columns for `create_images()`.",
      call. = FALSE
    )
  }
}

#' Validates the wrangled data frame has an `iso3` and `date columns`
validate_filter_df <- function(df) {
  if (!all(c("iso3", "date") %in% names(df))) {
    stop(
      "Data frame is required to have `iso3` and ",
      "`date` column for filtering.",
      call. = FALSE
    )
  }
}
