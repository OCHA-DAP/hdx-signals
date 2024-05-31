box::use(dplyr)
box::use(janitor)
box::use(purrr)
box::use(stringr)
box::use(rlang)
box::use(rlang[`!!`])
box::use(logger[log_info])

box::use(cs = ../utils/cloud_storage)
box::use(../utils/gmas_test_run)
box::use(./filter_test_data[filter_test_data])
box::use(./generate_campaign_content[generate_campaign_content])
box::use(./create_campaigns[create_campaigns])
box::use(./delete_campaign_content[delete_campaign_content])
box::use(./generate_alerts[generate_alerts])
box::use(./check_existing_signals[check_existing_signals])
box::use(./template_data)
box::use(../utils/hs_logger)

hs_logger$configure_logger()

#' Generate campaigns for any indicator
#'
#' Using all of the passed in data frames (alerts and wrangled) and functions,
#' generates the entire Mailchimp campaign for an indicator. Has many controls
#' internally to ensure that partially generated material is deleted when
#' errors occur. No campaigns are sent at the end of this, but are instead saved
#' Mailchimp and pending review.
#'
#' @param df_wrangled Data frame of wrangled data
#' @param df_raw Data frame of raw data
#' @param indicator_id ID of the indicator for the campaign, to match names in
#'     `input/indicator_mapping.parquet`, which can extract template folders and
#'     other info for campaigns.
#' @param alert_fn Function to generate alerts with.
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
#' @param test Whether or not to generate the signals for testing (defaults to
#'     `FALSE`. If `TRUE`, only a limited number of alerts are generated, based
#'     on the `test_filter` argument. If `GMAS_TEST_RUN` is `TRUE`, previews are
#'     generated using local HTML. Certain browsers cannot display local files
#'     correctly, so you may need to test on Mailchimp. If `GMAS_TEST_RUN` is
#'     `FALSE`, the campaigns are saved to Azure in
#'     `output/{indicator_id}/test/signals.parquet`. The campaign is uploaded
#'     to Mailchimp and then used for test visualization.
#' @param test_filter Used only if `test` is `TRUE`. If `NULL`, the default, then
#'     2 random signals from different countries are selected for testing. If you
#'     pass in a vector of `iso3` codes, then the latest signals from those
#'     countries are used for testing.
#'
#' @export
generate_signals <- function(
    df_wrangled,
    df_raw,
    indicator_id,
    alert_fn,
    plot_fn = NULL,
    map_fn = NULL,
    plot2_fn = NULL,
    other_images_fn = NULL,
    summary_fn = NULL,
    info_fn = NULL,
    first_run = FALSE,
    test = FALSE,
    test_filter = NULL) {
  # file name differs if testing or not
  fn_signals <- cs$signals_path(indicator_id, test)

  check_existing_signals(
    indicator_id = indicator_id,
    first_run = first_run,
    fn_signals = fn_signals,
    test = test
  )

  # generate the new alerts that will receive a campaign
  # filter out the data before generating new alerts
  df_alerts <- df_wrangled |>
    filter_test_data(
      test = test,
      test_filter = test_filter
    ) |>
    alert_fn() |>
    generate_alerts(
      indicator_id = indicator_id,
      first_run = first_run,
      test = test
    )

  # return empty data frame if alerts is empty
  if (nrow(df_alerts) == 0) {
    log_info(paste0("No signals created for ", indicator_id))
    return(
      template_data$signals_template
    )
  }


  # get content for the campaign
  df_campaign_content <- generate_campaign_content(
    df_alerts = df_alerts,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    plot_fn = plot_fn,
    map_fn = map_fn,
    plot2_fn = plot2_fn,
    other_images_fn = other_images_fn,
    summary_fn = summary_fn,
    info_fn = info_fn,
    empty = FALSE
  )

  # if first run, create multiple campaigns, one for each date
  # don't do this if testing, just create one test email
  if (first_run && !test) {
    df_campaigns <- df_campaign_content |>
      dplyr$group_by(
        date
      ) |>
      dplyr$group_split() |>
      purrr$map(
        .f = \(df) {
          create_campaigns(
            df_campaign_content = df,
            indicator_id = indicator_id,
            first_run = first_run,
            test = FALSE
          )
        }
      ) |>
      dplyr$bind_rows()
  } else {
    # otherwise, all new alerts are put into the same campaign
    df_campaigns <- create_campaigns(
      df_campaign_content = df_campaign_content,
      indicator_id = indicator_id,
      first_run = first_run,
      test = test
    )
  }

  # validate campaigns and delete content if any errors have occurred
  df_campaigns <- validate_campaigns(df_campaigns, df_campaign_content)

  df_campaigns <- dplyr$left_join(
    df_campaign_content,
    df_campaigns,
    by = c("iso3", "date")
  )

  cs$update_az_file(
    df_campaigns,
    fn_signals
  )

  log_info(paste0("Monitoring completed. ", nrow(df_campaigns), " signals generated for ", indicator_id))

  df_campaigns
}

#' Validate campaigns data
#'
#' Validates campaigns data to ensure template and campaign columns exist,
#' that no errors are present, and orders them correctly.
validate_campaigns <- function(df_campaigns, df_campaign_content) {
  df_check <- template_data$campaign_template

  # if any errors in the data frame or incorrect columns, delete content
  any_error <- any(dplyr$select(df_campaigns, -c(date, campaign_date)) == "ERROR", na.rm = TRUE)
  df_incorrect <- !janitor$compare_df_cols_same(df_campaigns, df_check, bind_method = "rbind")
  if (any_error || df_incorrect) {
    if (!gmas_test_run$gmas_test_run()) {
      delete_campaign_content(df_campaign_content)
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

  df_campaigns[, names(df_check)]
}
