box::use(dplyr)
box::use(tidyr)
box::use(stringr)

box::use(../email/mailchimp/delete)

#' Deletes campaign content from data frame
#'
#' Since campaign content is stored both on Mailchimp servers and in data
#' frames, we must make sure to delete the content from the servers prior to
#' simply deleting the data frame. This is useful at two junctures:
#'
#' - When a function creating content fails, in which case we need to delete
#' partially generated campaign content.
#' - When we want to recreate campaigns before sending, so we delete it before
#' recreation and replacement.
#'
#' No content is deleted from Mailchimp when `gmas_test_run()` is `TRUE`, but
#' the data frame is still filtered.
#'
#' @param df Data frame of campaign content
#'
#' @export
delete_campaign_content <- function(df) {
  # first, split out other images if present
  if ("other_images_ids" %in% names(df) && any(!is.na(df$other_images_ids))) {
    df <- tidyr$separate_wider_delim(
      data = df,
      cols = other_images_ids,
      delim = ";",
      names_sep = "_",
      too_few = "align_start"
    )
  }

  # get all file ids from the data frame and delete them
  delete_ids(
    df = df,
    cols = c("plot_id", "map_id", "plot2_id", dplyr$starts_with("other_images_id")),
    object_type = "file"
  )

  # delete the templates
  delete_ids(
    df = df,
    cols = dplyr$starts_with("template_id"),
    object_type = "template"
  )
  # delete the campaigns
  delete_ids(
    df = df,
    cols = dplyr$starts_with("campaign_id"),
    object_type = "campaign"
  )

  # now drop all the columns and return the data frame of alerts
  # only do this if available
  if (all(c("iso3", "value") %in% names(df))) {
    df |>
      dplyr$select(
        iso3:value
      )
  }
}

#' Deletes all IDs found in specified columns
#'
#' Extracts all IDs from a data frame and deletes them from the Mailchimp
#' servers.
delete_ids <- function(df, cols, object_type) {
  df_delete <- df |>
    dplyr$select(
      dplyr$any_of(
        cols
      )
    )

  if (ncol(df_delete) > 0) {
    df_delete |>
      tidyr$pivot_longer(
        dplyr$everything(),
        values_to = "id"
      ) |>
      dplyr$filter(
        !is.na(id) & id != "ERROR"
      ) |>
      dplyr$pull(id) |>
      unique() |>
      sapply(
        \(x) delete$mc_delete_object(x, object_type)
      )
  }
}
