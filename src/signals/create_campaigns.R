box::use(
  dplyr,
  glue,
  logger
)

box::use(
  src/email/components/email_body,
  src/email/components/create_template,
  src/email/mailchimp/templates,
  src/email/mailchimp/campaigns,
  src/email/mailchimp/custom_segmentation,
  src/utils/formatters,
  src/utils/get_signals_version,
  cs = src/utils/cloud_storage,
  src/utils/hs_dry_run,
  src/utils/hs_first_run
)

#' Create campaigns
#'
#' Creates Mailchimp campaign(s). The way segmentation works,
#' email campaigns are sent only to those subscribers who have subscribed to
#' regions where any alert was detected and subscribed to this specific indicator.
#'
#' If `HS_FIRST_RUN`, no emails are sent, just archived campaigns are generated.
#' If `HS_DRY_RUN` and `HS_LOCAL`, campaigns are previewed locally. When `HS_DRY_RUN`
#' is `TRUE` but `HS_LOCAL` is `FALSE`, campaigns are generated in Mailchimp
#' but only to internal emails tagged for testing.
#'
#' @param df_campaign_content Data frame with alerts information for generating the email.
#' @param indicator_id ID of the indicator, used for segmentation. {provider}_{type}
#'     is the standard format, such as `idmc_displacement`.
#'
#' @returns URL of the campaign. If two campaigns sent, then returns the URL
#' for `Medium concern` since it will contain all alerts for that run.
#'
#' @export
create_campaigns <- function(
    df_campaign_content,
    indicator_id) {
  if (nrow(df_campaign_content) == 0) {
    # if the content data frame is empty, just return empty campaigns
    return(
      dplyr$tibble(
        iso3 = character(),
        date = as.Date(integer(), origin = "1970-01-01"),
        template_id_archive = character(),
        template_id_email = character(),
        campaign_id_archive = character(),
        campaign_id_email = character(),
        campaign_url_archive = character(),
        campaign_url_email = character(),
        campaign_date = as.Date(integer(), origin = "1970-01-01")
      )
    )
  }
  first_run <- hs_first_run$hs_first_run()

  # create date for campaigns
  if (first_run) {
    campaign_date <- unique(df_campaign_content$date)
  } else {
    campaign_date <- Sys.Date()
  }

  # get template folder, title, and subject header
  campaign_details <- get_campaign_details(
    indicator_id = indicator_id,
    campaign_date = campaign_date
  )

  # generate email campaign with no conditional logic for the archive
  archive_df <- create_campaign(
    indicator_id = indicator_id,
    campaign_details = campaign_details,
    df_campaign_content = df_campaign_content,
    archive = TRUE,
    archive_url = "*|ARCHIVE|*",
    names_paste = "_archive"
  )

  if (!first_run) {
    email_df <- create_campaign(
      indicator_id = indicator_id,
      campaign_details = campaign_details,
      df_campaign_content = df_campaign_content,
      archive = FALSE,
      archive_url = unique(archive_df$campaign_url_archive),
      names_paste = "_email"
    )
  } else {
    email_df <- dplyr$tibble(
      template_id_email = NA_character_,
      campaign_id_email = NA_character_,
      campaign_url_email = NA_character_,
      .rows = nrow(df_campaign_content)
    )
  }

  # get campaigns and iso3/date for joining back to content
  df_campaigns <- dplyr$bind_cols(
    dplyr$select(df_campaign_content, iso3, date),
    archive_df,
    email_df
  ) |>
    dplyr$mutate(
      campaign_url_archive = paste(campaign_url_archive, iso3, sep = "#")
    )

  # add the date of the campaign
  df_campaigns$campaign_date <- campaign_date
  df_campaigns$signals_version <- get_signals_version$get_signals_version()
  df_campaigns
}

#' Create a campaign
#'
#' Creates a Mailchimp campaign. First, an email body is created
#' and used to generate a new Mailchimp template for the campaign. Segmentation
#' is done based on the indicator and locations, and the empty segment is used
#' if this is an historical campaign. Finally, a new campaign is created in
#' Mailchimp and sent out.
#'
#' `HS_DRY_RUN` is used to determine if emails should be sent to wide audience
#' or internally for testing, and previews are generated only if `TRUE`.
#'
#' @param indicator_id ID of the indicator, used for segmentation. {provider}_{type}
#'     is the standard format, such as `idmc_displacement`.
#' @param campaign_details Details for the campaign, such as the subject, title,
#'     and campaign title to be used in campaign creation.
#' @param df_campaign_content Data frame with alerts information for generating the email.
#' @param archive Whether or not this email is designed for the archive. If `TRUE`,
#'     no conditional logic is included in the email, and the email is previewed
#'     if in a dry run. If `FALSE`, conditional logic is used and emails are
#'     sent to segments.
#' @param archive_url URL for the archive.
#' @param names_paste String to paste on the end of the names, either `_archive`,
#'     `_hc`, or `_all`.
#'
#' @returns URL of the campaign
create_campaign <- function(
    indicator_id,
    campaign_details,
    df_campaign_content,
    archive,
    archive_url,
    names_paste) {
  template_id <- tryCatch(
    {
      # create the email template and add to Mailchimp
      body <- email_body$create_body(
        campaign_details = campaign_details,
        df_campaign_content = df_campaign_content,
        use_conditions = !archive
      )

      template <- create_template$create_template(
        body = body,
        banner_url = campaign_details$banner_url,
        archive_url = archive_url
      )

      templates$mc_add_template(
        html = template,
        folder = campaign_details$folder,
        # only preview the archive template, not those with conditional logic
        # don't preview if running from within GitHub Actions
        preview = archive && hs_dry_run$hs_dry_run() && interactive()
      )
    },
    error = function(e) {
      logger$log_error(e$message)
      "ERROR"
    }
  )

  # now separately run tryCatch so we can delete template if need be
  if (template_id != "ERROR") {
    id_url <- tryCatch(
      {
        # create segmentation
        if (!archive) {
          segments <- custom_segmentation$mc_email_segment(
            indicator_id = indicator_id,
            iso3 = df_campaign_content$iso3
          )
        } else {
          # assign to archive segment for historic campaigns so URL created but
          # email is only sent to HDX Signals email (we have to send email
          # for archive links to be fully functional)
          segments <- custom_segmentation$mc_archive_segment()
        }

        campaigns$mc_add_campaign(
          subject_line = campaign_details$subject,
          preview_text = paste(df_campaign_content$location, collapse = ", "),
          title = glue$glue(campaign_details$campaign_title),
          recipients = segments,
          template_id = as.numeric(template_id),
          folder = campaign_details$folder
        )
      },
      error = function(e) {
        logger$log_error(e$message)
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

  # change the names
  names(df_campaigns) <- paste0(names(df_campaigns), names_paste)
  df_campaigns
}

#' Get subject and title for campaigns
#'
#' Gets the subject and title for campaigns. Subjects are simply:
#' `New Signal: INDICATOR, DATE`.
#' The titles are for the file system, and so are similar, except also use
#' `name_paste` to differentiate between
get_campaign_details <- function(indicator_id, campaign_date) {
  df_ind <- cs$read_az_file("input/indicator_mapping.parquet") |>
    dplyr$filter(
      indicator_id == !!indicator_id
    )

  dry_run <- hs_dry_run$hs_dry_run()

  list(
    title = df_ind$indicator_subject,
    subject = paste0(
      if (dry_run) "TEST - " else "",
      "Signal: ",
      df_ind$indicator_subject,
      ", ",
      formatters$format_date(Sys.Date())
    ),
    campaign_title = paste0(
      "hdx_signals_",
      if (dry_run) "test_" else "",
      indicator_id,
      format(campaign_date, "_%Y_%m_%d"),
      "{names_paste}" # for glue within create_campaign()
    ),
    folder = df_ind$mc_folder,
    banner_url = df_ind$banner_url
  )
}
