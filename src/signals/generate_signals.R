box::use(
  dplyr,
  janitor,
  purrr,
  stringr,
  rlang,
  logger
)

box::use(
  src/signals/filter_test_data,
  src/signals/generate_campaign_content,
  src/signals/create_campaigns,
  src/signals/delete_campaign_content,
  src/signals/generate_alerts,
  src/signals/check_existing_signals,
  src/signals/template_data,
  src/signals/validate_indicator,
  cs = src/utils/cloud_storage,
  src/utils/hs_logger,
  src/utils/hs_local,
  src/utils/hs_dry_run,
  src/utils/hs_first_run,
  src/utils/update_coverage
)

#' Generate campaigns for any indicator
#'
#' Using all of the passed in data frames (alerts and wrangled) and functions,
#' generates the entire Mailchimp campaign for an indicator. Has many controls
#' internally to ensure that partially generated material is deleted when
#' errors occur. No campaigns are sent at the end of this, but are instead saved
#' Mailchimp and pending review.
#'
#' If `HS_FIRST_RUN` is `TRUE`, then no emails will be generated, simply archived
#' campaigns. This is done by splitting the campaigns dataset by date and
#' generating alerts across each date individually. If it is not the first run,
#' then the entire alerts data frame is converted into a single campaign during
#' monitoring.
#'
#' If `HS_DRY_RUN` is `TRUE`, only a limited number of alerts are generated, based
#' on the `dry_run_filter` argument. If `HS_LOCAL` is `TRUE`, previews are
#' generated using local HTML. If `HS_LOCAL` is `FALSE`, the campaigns are saved
#' to Azure in `output/{indicator_id}/test/signals.parquet`. The campaign is uploaded
#' to Mailchimp and then used for test visualization. If `HS_DRY_RUN` is `FALSE`,
#' then monitoring is done normally.
#'
#' @param ind_module Indicator module that contains functions to retrieve data,
#'     wrangle it, and generate signals for relevant locations.
#' @param dry_run_filter Used only if `HS_DRY_RUN` is `TRUE`. If `NULL`, the default,
#'     then 2 random signals from different locations are selected for testing.
#'     If you pass in a vector of `iso3` codes, then the latest signals from those
#'     locations are used for testing.
#'
#' @export
generate_signals <- function(
    ind_module,
    dry_run_filter = NULL) {
  # validate that module
  validate_indicator$validate_indicator(ind_module)

  # setup run and retrieve/wrangle data
  hs_logger$monitoring_log_setup(ind_module$indicator_id)
  df_raw <- ind_module$raw()
  df_wrangled <- ind_module$wrangle(df_raw)

  # update coverage data to ensure locations_metadata up to date
  update_coverage$update_coverage(
    indicator_id = ind_module$indicator_id,
    iso3 = df_wrangled$iso3
  )

  # file name differs if testing or not
  fn_signals <- cs$signals_path(ind_module$indicator_id, hs_dry_run$hs_dry_run())

  check_existing_signals$check_existing_signals(
    indicator_id = ind_module$indicator_id,
    fn_signals = fn_signals
  )

  # generate the new alerts that will receive a campaign
  # filter out the data before generating new alerts
  df_alerts <- df_wrangled |>
    ind_module$alert() |>
    filter_test_data$filter_test_data(
      dry_run_filter = dry_run_filter
    ) |>
    generate_alerts$generate_alerts(
      indicator_id = ind_module$indicator_id
    )
  # return empty data frame if alerts is empty
  if (nrow(df_alerts) == 0) {
    logger$log_info(paste0("No signals created for ", ind_module$indicator_id))
    return(
      template_data$signals_template
    )
  }

  # get content for the campaign
  df_campaign_content <- generate_campaign_content$generate_campaign_content(
    df_alerts = df_alerts,
    df_wrangled = df_wrangled,
    df_raw = df_raw,
    ind_module = ind_module,
    empty = FALSE
  )

  # if first run, create multiple campaigns, one for each date
  # don't do this if testing, just create one test email
  if (hs_first_run$hs_first_run()) {
    df_campaigns <- df_campaign_content |>
      dplyr$group_by(
        date
      ) |>
      dplyr$group_split() |>
      purrr$map(
        .f = \(df) {
          create_campaigns$create_campaigns(
            df_campaign_content = df,
            indicator_id = ind_module$indicator_id
          )
        }
      ) |>
      dplyr$bind_rows()
  } else {
    # otherwise, all new alerts are put into the same campaign
    df_campaigns <- create_campaigns$create_campaigns(
      df_campaign_content = df_campaign_content,
      indicator_id = ind_module$indicator_id
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

  logger$log_info(
    paste0(
      "Monitoring completed. ",
      nrow(df_campaigns),
      " signals generated for ",
      ind_module$indicator_id
    )
  )

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
    if (!hs_local$hs_local()) {
      delete_campaign_content$delete_campaign_content(df_campaign_content)
      delete_campaign_content$delete_campaign_content(df_campaigns)
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
