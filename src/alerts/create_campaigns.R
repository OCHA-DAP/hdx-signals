box::use(dplyr)
box::use(rlang[`!!`])
box::use(purrr)

box::use(cs = ../utils/cloud_storage)
box::use(../email/components/email_body)
box::use(../email/components/create_template)
box::use(../email/mailchimp/templates)
box::use(../email/mailchimp/campaigns)
box::use(../email/mailchimp/custom_segmentation)
box::use(../utils/format_date[format_date])
box::use(../email/mailchimp/delete)

#' Create campaigns
#'
#' Creates Mailchimp campaign(s). The way segmentation works,
#' email campaigns are sent separately to the different values of `alert_level`
#' for a specific `indicator_id`. Thus, this calls `create_campaign()` separately
#' for different subscribers based on `subscription_option`.
#'
#' @param df_campaign_content Data frame with alerts information for generating the email.
#' @param indicator_id ID of the indicator, used for segmentation. {provider}_{type}
#'     is the standard format, such as `idmc_displacement`.
#' @param first_run Whether or not this is the first run for an indicator. On the
#'     first run, no alerts are sent, just archived campaigns are generated.
#'
#' @returns URL of the campaign. If two campaigns sent, then returns the URL
#' for `Medium concern` since it will contain all country alerts for that run.
#'
#' @export
create_campaigns <- function(
    df_campaign_content,
    indicator_id,
    first_run = FALSE
) {
  # create date for campaigns
  if (first_run) {
    campaign_date <- df_campaign_content$date
  } else {
    campaign_date <- Sys.Date()
  }

  # get template folder, title, and subject header
  campaign_details <- get_campaign_details(indicator_id, unique(campaign_date))

  # generate email campaign with no conditional logic for the archive
  archive_df <- create_campaign(
    indicator_id = indicator_id,
    campaign_details = campaign_details,
    subscription_option = "Receive all alerts",
    df_campaign_content = df_campaign_content,
    archive = TRUE,
    archive_url = "*|ARCHIVE|*",
    names_paste = "_archive"
  )

  if (!first_run && "High concern" %in% df_campaign_content$alert_level) {
    # only high concern alerts for the high concern crowd
    hc_df <- create_campaign(
      indicator_id = indicator_id,
      campaign_details = campaign_details,
      subscription_option = "Only receive alerts of high concern",
      df_campaign_content = dplyr$filter(df_campaign_content, alert_level == "High concern"),
      archive = FALSE,
      archive_url = archive_id_url$campaign_url_archive,
      names_paste = "_hc"
    )
  } else {
    hc_df <- dplyr$tibble(
      campaign_row_number = df_campaign_content$campaign_row_number,
      template_id_hc = NA_character_,
      campaign_id_hc = NA_character_,
      campaign_url_hc = NA_character_,
      .rows = nrow(df_campaign_content)
    )
  }

  if (!first_run) {
    all_df <- create_campaign(
      # all alerts goes to those who subscribed
      indicator_id = indicator_id,
      campaign_details = campaign_details,
      subscription_option = "Receive all alerts",
      df_campaign_content = df_campaign_content,
      archive = FALSE,
      archive_url = archive_id_url$campaign_url_archive,
      names_paste = "_all"
    )
  } else {
    all_df <- dplyr$tibble(
      campaign_row_number = df_campaign_content$campaign_row_number,
      template_id_all = NA_character_,
      campaign_id_all = NA_character_,
      campaign_url_all = NA_character_,
      .rows = nrow(df_campaign_content)
    )
  }
  df_campaigns <- purrr$reduce(
    .x = list(archive_df, hc_df, all_df),
    .f = \(x, y) dplyr$left_join(x, y, by = "campaign_row_number")
  ) |>
    dplyr$select(
      -campaign_row_number
    )

  df_campaigns$campaign_date <- campaign_date
  df_campaigns
}

#' Create a campaign
#'
#' Creates a Mailchimp campaign. First, an email body is created
#' and used to generate a new Mailchimp template for the campaign. Segmentation
#' is done based on the indicator and countries, and the empty segment is used
#' if this is an historical campaign. Finally, a new campaign is created in
#' Mailchimp and sent out.
#'
#' @param indicator_id ID of the indicator, used for segmentation. {provider}_{type}
#'     is the standard format, such as `idmc_displacement`.
#' @param campaign_details Details for the campaign, such as the subject, title,
#'     and campaign title to be used in campaign creation.
#' @param subscription_option Subcription option for subscribers that will receive
#'     the email. Either those only wanting high concern alerts or all subscribers.
#'     Used in segmentation.
#' @param df_campaign_content Data frame with alerts information for generating the email.
#' @param archive Whether or not this email is designed for the archive. If `TRUE`,
#'     no conditional logic is included in the email, and the email is previewed
#'     if in a test run. If `FALSE`, conditional logic is used and emails are
#'     sent to segments.
#' @param archive_url URL for the archive.
#' @param names_paste String to paste on the end of the names, either `_archive`,
#'     `_hc`, or `_all`.
#'
#' @returns URL of the campaign
create_campaign <- function(
    indicator_id,
    campaign_details,
    subscription_option,
    df_campaign_content,
    archive,
    archive_url,
    names_paste
) {
  template_id <- tryCatch(
    {
      # create the email template and add to Mailchimp
      body <- email_body$create_body(
        title = campaign_details$title,
        iso3 = df_campaign_content$iso3,
        country = df_campaign_content$country,
        plot_title = df_campaign_content$plot_title,
        plot_url = df_campaign_content$plot_url,
        map_title = df_campaign_content$map_title,
        map_url = df_campaign_content$map_url,
        plot2_title = df_campaign_content$plot2_title,
        plot2_url = df_campaign_content$plot2_url,
        other_images_urls = df_campaign_content$other_images_urls,
        other_images_captions = df_campaign_content$other_images_captions,
        summary = df_campaign_content$summary_long,
        further_information = df_campaign_content$further_information,
        use_conditions = !archive
      )

      template <- create_template$create_template(
        body = body,
        archive_url = archive_url
      )

      templates$mc_add_template(
        html = template,
        folder = campaign_details$folder,
        preview = archive # only preview the archive template, not those with conditional logic
      )
    },
    error = function(e) {
      "ERROR"
    }
  )

  # now separately run tryCatch so we can delete template if need be
  if (template_id != "ERROR") {
    id_url <- tryCatch(
      {
        # create segmentation
        if (!archive) {
          segments <- custom_segmentation$mc_segment_conditions(
            indicator_id = indicator_id,
            iso3 = df_campaign_content$iso3,
            subscription_option = subscription_option
          )
        } else {
          # assign to empty segment for historic campaigns so URL created but no emails sent
          segments <- custom_segmentation$mc_empty_segment()
        }

        campaigns$mc_add_campaign(
          subject_line = campaign_details$subject,
          preview_text = paste(df_campaign_content$country, collapse = ", "),
          title = campaign_details$campaign_title,
          recipients = segments,
          template_id = template_id
        )
      },
      error = function(e) {
        # template_id will be deleted in later cleanup
        list(id = "ERROR", url = "ERROR")
      }
    )
  } else {
    id_url <- list(id = "ERROR", url = "ERROR")
  }

  # create the campaigns data frame with the same number of rows
  df_campaigns <- dplyr$tibble(
    template_id = template_id,
    campaign_id = id_url[["id"]],
    campaign_url = id_url[["url"]],
    .rows = nrow(df_campaign_content)
  )

  # change the names and then add back in row number for joining together
  names(df_campaigns) <- paste0(names(df_campaigns), names_paste)
  df_campaigns$campaign_row_number <- df_campaign_content$campaign_row_number
  df_campaigns
}

#' Get subject and title for campaigns
#'
#' Gets the subject and title for campaigns. Subjects are simply:
#' `HDX Signals: INDICATOR, DATE`.
#' The titles are for the file system, and so are similar, except also use
#' `name_paste` to differentiate between
get_campaign_details <- function(indicator_id, campaign_date) {
  df_ind <- cs$read_az_file("input/indicator_mapping.parquet") |>
    dplyr$filter(
      indicator_id == !!indicator_id
    )

  list(
    title = df_ind$indicator_subject,
    subject = paste0(
      "HDX Signals: ",
      df_ind$indicator_subject,
      ", ",
      Sys.Date()
    ),
    campaign_title = paste0(
      "hdx_signals_",
      indicator_id,
      format(campaign_date, "_%Y_%m_%d"),
      "{names_paste}" # for glue within create_campaign()
    ),
    folder = df_ind$mc_folder
  )
}
