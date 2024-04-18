box::use(stringr)
box::use(utils)
box::use(purrr)
box::use(dplyr)

box::use(cs = ../utils/cloud_storage)
box::use(./delete_campaign_content[delete_campaign_content])
box::use(../email/mailchimp/campaigns)

#' Triage signals generated automatically
#'
#' Go through the automatically generated signals for an indicator, stored in
#' `output/{indicator_id}/signals.parquet` files. For all unique campaign in the
#' data, the `campaign_url_archive` is previewed. For each campaign, the user can select
#' to:
#'
#' - `APPROVE`: Approve the campaign, in which case the campaign is sent
#' (if necessary), the rows are deleted from
#' `output/{indicator_id}/signals.parquet` and added to `output/signals.parquet`
#'
#' - `DELETE` the campaign content, which will delete the campaign content but
#' leave the alerts information. This is so you don't recalculate when we would
#' signal, just the visuals and other campaign information in the email. This will
#' delete all campaign content from Mailchimp and then remove those columns from
#' `output/{indicator_id}/signals.parquet`. You will have to run
#' `generate_signals(..., overwrite_content = TRUE)`.
#'
#' - `Any other user input`: Do nothing, in which you can decide later manually what to do, and use
#' `delete_campaign_content()` or `send_signals()` yourself manually.
#'
#' There is no way to completely delete the alerts, which would essentially
#' determine that we do not want to send an alert at all. This would mean changing
#' our alert methodology or manually bypassing it, so you will need to manually
#' do this given how critical that decision is.
#'
#' During monitoring, signals are sent out as a single campaign. However, for
#' `first_run` builds of historic campaign archives, campaigns are developed
#' individually based on the date. However, you still have to manually triage
#' all campaigns at one point in time and give a single command to `APPROVE` or
#' `DELETE` the entire run of signals.
#'
#' @param indicator_id Indicator ID mapped in `input/indicator_mapping.parquet`
#' @param n_campaigns When previewing the campaigns, how many to open at one
#'      time. Defaults to `10`, which is fine for most runs, but if `first_run`
#'      creates many at a time, you may need to be increase or decrease based
#'      on your preference.
#'
#' @export
triage_signals <- function(indicator_id, n_campaigns = 10) {
  fn_signals <- paste0("output/", indicator_id, "/signals.parquet")
  df <- get_signals_df(fn_signals)
  preview_signals(df = df, n_campaigns = n_campaigns)
  approve_signals(df = df, fn_signals = fn_signals)
}

#' Check the signals data frame
#'
#' Checks the signals data frame, and if it is non-empty, returns it.
get_signals_df <- function(fn_signals) {
  # check that there signals to triage
  if (fn_signals %in% cs$az_file_detect()) {
    df_ind_signals <- cs$read_az_file(fn_signals)
    nrow_ind_signals <- nrow(df_ind_signals)
  } else {
    nrow_ind_signals <- 0
  }

  if (nrow_ind_signals == 0) {
    stop(
      stringr$str_wrap(
        paste0(
          "There are no signals to triage in '",
          fn_signals,
          "'. You can run `generate_signals()` manually, but think carefully ",
          "before bypassing the automated runs just because the signals are empty."
        )
      ),
      call. = FALSE
    )
  }

  df_ind_signals
}

#' Preview signals
#'
#' Preview `n_campaigns` at a time. These are opened in the browser and the user
#' just needs to press any button to continue and preview additional ones.
#'
#' @param df Signals data frame
#' @param n_campaigns Number of campaigns to preview at a time
preview_signals <- function(df, n_campaigns) {
  urls <- df$campaign_url_archive
  url_list <- split(urls, ceiling(seq_along(urls) / n_campaigns))
  purrr$walk(
    .x = url_list,
    .f = preview_campaign_urls
  )
}

#' Preview campaign URLs
#'
#' Preview campaign URLs. Vector of URLs passed in are opened in your browser.
#' The user needs to press any input to continue.
#'
#' @param campaign_urls Vector of campaign URLs to preview
preview_campaign_urls <- function(campaign_urls) {
  purrr$walk(
    .x = campaign_urls,
    .f = utils::browseURL
  )

  readline("Press any key to continue.")
}

#' Approve (or not) campaigns
#'
#' @param df Signals data frame
#' @param fn_signals File name to the signals data
approve_signals <- function(df, fn_signals) {
  user_command <- readline(
    paste0(
      "Tell us what you want to do with the following commands:\n\n",
      "APPROVE: Send campaigns and add to `output/signals.parquet`\n",
      "DELETE: Delete the campaign content, so you can recreate later.\n",
      "Anything other input: Do nothing, so you can decide later."
    )
  )
  if (user_command == "APPROVE") {
    send_signals(df)
    df_core_signals <- dplyr$bind_rows(
      read_core_signals(),
      df
    )
    # adds the indicator signals data to the core file
    # and then empties the indicator one
    cs$update_az_file(df_core_signals, "output/signals.parquet")
    cs$update_az_file(df[0,], fn_signals)

  } else if (user_command == "DELETE") {
    df_deleted <- delete_campaign_content(df)
    cs$update_az_file(
      df = df_deleted,
      name = fn_signals
    )
  }
}


#' Sends the signals in the indicator data frame
#'
#' Sends the signals in the data frame. The signals that actually need sending
#' are those that have non-missing `campaign_id_email` values, because the archives
#' are never sent out as they have empty segments.
#'
#' @param df Indicator signals data frame
send_signals <- function(df) {
  # first send out the archive campaigns to activate the URLs
  send_signals_email(df, "campaign_id_archive")
  send_signals_email(df, "campaign_id_email")
}

#' Send out emails based on ID column
send_signals_email <- function(df, id_col) {
  df |>
    dplyr$filter(
      !is.na(.data[[id_col]])
    ) |>
    dplyr$pull(
      .data[[id_col]]
    ) |>
    unique() |>
    purrr$walk(
      .f = campaigns$mc_send_campaign
    )
}

#' Read core signals data frame
#'
#' Reads the data frame if it exists. Just returns an empty data frame if it
#' doesn't work.
read_core_signals <- function() {
  if ("output/signals.parquet" %in% cs$az_file_detect()) {
    cs$read_az_file("output/signals.parquet")
  } else {
    data.frame()
  }
}
