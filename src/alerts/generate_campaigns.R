box::use(dplyr)
box::use(janitor)
box::use(purrr)
box::use(stringr)
box::use(rlang)

box::use(cs = ../utils/cloud_storage)
box::use(../utils/gmas_test_run)
box::use(./generate_campaign_content[generate_campaign_content])
box::use(./create_campaigns[create_campaigns])
box::use(./delete_campaign_content[delete_campaign_content])

#' Generate campaigns for any indicator
#'
#' Using all of the passed in data frames (alerts and wrangled) and functions,
#' generates the entire Mailchimp campaign for an indicator. Has many controls
#' internally to ensure that partially generated material is deleted when
#' errors occur. No campaigns are sent at the end of this, but are instead saved
#' Mailchimp and pending review.
#'
#' @param df_alerts Data frame of alerts
#' @param df_wrangled Data frame of wrangled data
#' @param fn_df_campaigns File name for the campaigns data frame
#' @param indicator_id ID of the indicator for the campaign, to match names in
#'     `input/indicator_mapping.parquet`, which can extract template folders and
#'     other info for campaigns.
#' @param plot_fn Plotting function
#' @param map_fn Mapping function
#' @param plot2_fn Second plotting function
#' @param other_images_fn Function to embed other images
#' @param summary_fn Function to generate a summary
#' @param info_fn Function to add additional information
#' @param first_run Whether or not this is the first run for the indicator. If it
#'     is, then no emails will be generated, simply archived campaigns. This is
#'     done by splitting the campaigns dataset by data and generating alerts
#'     across each date individually. If it is not the first run, then the
#'     entire alerts data frame is converted into a single campaign during monitoring.
#'
#' @export
generate_campaigns <- function(
    df_alerts,
    df_wrangled,
    fn_df_campaigns,
    indicator_id,
    plot_fn = NULL,
    map_fn = NULL,
    plot2_fn = NULL,
    other_images_fn = NULL,
    summary_fn = NULL,
    info_fn = NULL,
    first_run = FALSE
) {
  if (fn_df_campaigns %in% cs$az_file_detect()) {
    if (nrow(cs$read_az_file(fn_df_campaigns)) != 0) {
      stop(
        "You must first triage all existing campaigns in '",
        fn_df_campaigns,
        "' before generating new ones from alerts.",
        call. = FALSE
      )
    }
  }

  # get content for the campaign
  df_campaign_content <- generate_campaign_content(
    df_alerts = df_alerts,
    df_wrangled = df_wrangled,
    plot_fn = plot_fn,
    map_fn = map_fn,
    plot2_fn = plot2_fn,
    other_images_fn = other_images_fn,
    summary_fn = summary_fn,
    info_fn = info_fn,
    empty = FALSE
  )

  # upload content to Mailchimp in case campaigns sending fails
  cs$update_az_file(df_campaign_content, fn_df_campaigns)

  # save up all email content in case creating the template and campaign fails
  # if first run, create multiple campaigns, one for each date
  if (first_run) {
    df_campaigns <- df_campaign_content |>
      dplyr$group_by(
        date
      ) |>
      dplyr$group_split() |>
      purrr$map(
        .f = \(df) create_campaigns(
          df_campaign_content = df,
          indicator_id = indicator_id,
          first_run = first_run
        )
      ) |>
      dplyr$bind_rows()
  } else {
    df_campaigns <- create_campaigns(
      df_campaign_content = df_campaign_content,
      indicator_id = indicator_id,
      first_run = first_run
    )
  }

  df_campaigns <- validate_campaigns(df_campaigns)

  df_campaigns <- dplyr$bind_cols(
    df_campaign_content,
    df_campaigns
  )

  cs$update_az_file(
    df_campaigns,
    fn_df_campaigns
  )
  df_campaigns
}

#' Validate campaigns data
#'
#' Validates campaigns data to ensure template and campaign columns exist,
#' that no errors are present, and orders them correctly.
validate_campaigns <- function(df_campaigns) {
  df_check <- dplyr$tibble(
    template_id_archive = NA_character_,
    template_id_hc = NA_character_,
    template_id_all = NA_character_,
    campaign_id_archive = NA_character_,
    campaign_id_hc = NA_character_,
    campaign_id_all = NA_character_,
    campaign_url_archive = NA_character_,
    campaign_url_hc = NA_character_,
    campaign_url_all = NA_character_,
    campaign_date = as.Date(x = integer(0), origin = "1970-01-01")
  )

  if (any(dplyr$select(df_campaigns, -campaign_date) == "ERROR", na.rm = TRUE) || !janitor$compare_df_cols_same(df_campaigns, df_check, bind_method = "rbind")) {
    if (!gmas_test_run$gmas_test_run()) {
      delete_campaign_content(df_campaigns)
      rlang$abort(
        stringr$str_wrap(
          paste0(
            "Errors generated when finalizing templates and campaigns, or incorrect ",
            "columns and types were present. The content was ",
            "successfully created and stored in the campaigns data frame on Azure. ",
            "Any templates and campaigns created have been deleted from Mailchimp. ",
            "Explore the errors generated in `create_campaigns()`, fix, and try again."
          )
        ),
        call = NULL
      )
    } else {
      rlang$abort(
        message = stringr$str_wrap(
          paste0(
            "Errors generated when finalizing templates and campaigns, or incorrect ",
            "columns and types were present. The content was ",
            "successfully created, but errors were generated during template creation. ",
            "Explore the errors generated in `create_campaigns()`, fix, and try again."
          )
        ),
        call = NULL
      )
    }
  }

  df_campaigns[,names(df_check)]
}


