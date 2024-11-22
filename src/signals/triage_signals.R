box::use(
  stringr,
  purrr,
  dplyr,
  glue
)

box::use(
  src/signals/delete_campaign_content,
  src/signals/template_data,
  src/email/mailchimp/campaigns,
  cs = src/utils/cloud_storage,
  src/utils/get_env,
  src/utils/push_hdx
)

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
#' delete all campaign content from Mailchimp and then delete all of the rows
#' from `output/{indicator_id}/signals.parquet`.
#'
#' - `DO_NOTHING` Do nothing, in which you can decide later manually what to do, and use
#' `delete_campaign_content()` or `send_signals()` yourself manually.
#'
#' - `Any other user input`: the user will be asked again what's the desired action to be taken.
#'
#' The`APPROVE`, `DELETE` and `APPROVE` command will require a double confirmation given the criticality of the action.
#' To confirm the user needs to type `I CONFIRM`
#'
#' There is no way to completely delete the alerts, which would essentially
#' determine that we do not want to send an alert at all. This would mean changing
#' our alert methodology or manually bypassing it, so you will need to manually
#' do this given how critical that decision is.
#'
#' During monitoring, signals are sent out as a single campaign. However, for
#' `HS_FIRST_RUN` builds of historic campaign archives, campaigns are developed
#' individually based on the date. However, you still have to manually triage
#' all campaigns at one point in time and give a single command to `APPROVE` or
#' `DELETE` the entire run of signals.
#'
#' `triage_signals()` accepts the explicit `test` argument rather than looking
#' for `HS_DRY_RUN` because the function is intended for interactive use. Users
#' may want to look to triage signals in `output/{indicator}/test/signals.parquet`,
#' by setting `test` to `TRUE`. However, they will need their own `HS_LOCAL`
#' env variable set to `FALSE` in order to `DELETE` or `APPROVE` these test signals.
#'
#' @param indicator_id Indicator ID mapped in `input/indicator_mapping.parquet`
#' @param n_campaigns When previewing the campaigns, how many to open at one
#'      time. Defaults to `10`, which is fine for most runs, but if `HS_FIRST_RUN`
#'      creates many at a time, you may need to be increase or decrease based
#'      on your preference. If `0`, no signals are previewed.
#' @param test Whether or not we are triaging signals in the
#'      `output/{indicator}/test/signals.parquet` files. You can still send dry
#'      run emails and delete them in the same way as normal ones, but the signals
#'      file is not sent to `output/signals.parquet`
#'
#' @export
triage_signals <- function(indicator_id, n_campaigns = 10, test = FALSE) {
  fn_signals <- cs$signals_path(indicator_id, test)
  df <- get_signals_df(fn_signals)
  preview_signals(df = df, n_campaigns = n_campaigns)
  user_input <- approve_signals(df = df, fn_signals = fn_signals, test = test, indicator_id = indicator_id)
  dispatch_signals(df = df, fn_signals = fn_signals, test = test, user_command = user_input)
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
#' just needs to press any button to continue and preview additional ones. If
#' `n_campaigns` is `0`, no previews are generated.
#'
#' @param df Signals data frame
#' @param n_campaigns Number of campaigns to preview at a time
preview_signals <- function(df, n_campaigns) {
  if (n_campaigns == 0) {
    return(invisible(NULL))
  }
  # get unique campaign URLs (so exclude jump link to country)
  urls <- df$campaign_url_archive |>
    stringr$str_remove("#[A-Z]{3}$") |>
    unique()
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
#' @param test Whether or not the signals were in the test folder.
approve_signals <- function(df, fn_signals, test, indicator_id) {
  user_command  <- "init"
  while (!(user_command %in% c("APPROVE", "DELETE", "ARCHIVE", "DO_NOTHING"))) {
    # send message to user
    msg <- glue$glue(
                     "Tell us what you want to do with the following commands:

                      APPROVE: Send campaigns{if (test) '' else ' and add to `output/signals.parquet`'}
                      DELETE: Delete the campaign content and `output/{indicator_id}/signals.parquet` file
                      so you can recreate later.
                      ARCHIVE: Delete the email campaign, but move the alert to `output/signals.parquet`
                      DO_NOTHING: Do nothing, so you can decide later.")
    print(msg)
    user_command <- readline(prompt = "Your command: ")
  }
  if (user_command %in% c("APPROVE", "DELETE", "ARCHIVE")) {
    # send message to user
    cat(
      "You typed the command: ", user_command, "\n",
      "Given the criticality of the action please type I CONFIRM to proceed with the action selected",
      sep = ""
    )

    user_command_confirmation <- readline(prompt = "Your command: ")
    if (user_command_confirmation != "I CONFIRM") {
      stop(glue$glue("The process was not confirmed, you will need to re-run `triage_signals()` to {user_command} the
                     signal and then confirm it."),
        call. = FALSE
      )
    }
  }
  user_command
}

#' Dispatch or not campaigns based on user input
#'
#' @param df Signals data frame
#' @param fn_signals File name to the signals data
#' @param test Whether or not the signals were in the test folder.
#' @param user_command command provided by the user
dispatch_signals <- function(df, fn_signals, test, user_command) {
  if (user_command %in% c("APPROVE", "ARCHIVE")) {
    if (user_command == "ARCHIVE") {
      # delete the email template and campaigns, but not the content
      delete_campaign_content$delete_campaign_content(
        df = dplyr$select(df, dplyr$ends_with("_email"))
      )
      # ensure the email columns are empty
      df <- dplyr$mutate(
        df,
        dplyr$across(
          .cols = dplyr$ends_with("_email"),
          .fns = \(x) NA_character_
        )
      )
    }

    # add triage information to the data before joining to core
    # do this now so we test this process when `test = TRUE`
    df$triage_approver <- get_env$get_env("HS_ADMIN_NAME")
    df$triage_time <- Sys.time()

    df_core_signals <- dplyr$bind_rows(
      read_core_signals(),
      df
    )

    send_signals(df)

    # if not testing, move everything to the core signals dataset
    if (!test) {
      # adds the indicator signals data to the core file
      # saves a reduced version as CSV to dev for pipelining to HDX
      # and then empties the indicator one
      cs$update_az_file(df_core_signals, "output/signals.parquet")
      save_core_signals_hdx(df_core_signals)
      cs$update_az_file(df[0, ], fn_signals)
    } else {
      if (user_command == "ARCHIVE") {
        action_selected <= "archived"
      } else {
        action_selected <- "sent"
      }
      new_input <- readline(
        paste0(glue$glue("You have {action_selected} your test campaigns. If you want to delete the\n"),
               "test campaigns file ",
               fn_signals,
               " and its content from Mailchimp, type DELETE.")
      )
      if (new_input == "DELETE") {
        df_deleted <- delete_campaign_content$delete_campaign_content(df)
        cs$update_az_file(df_deleted, fn_signals)
        message("The campaigns file have been succesfully deleted.")
      } else {
        message(
          "You have not deleted the content in ",
          fn_signals,
          " or removed the content from Mailchimp.\n Remember to do so in the ",
          "future.",
          call. = FALSE
        )
      }
    }
  } else if (user_command == "DELETE") {
    # replace the campaign content with the deleted stuff
    df_deleted <- delete_campaign_content$delete_campaign_content(df)

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

#' Save core signals data for HDX
#'
#' Filters the core signals data for HDX, saves to `prod` container in Azure
#' and then pushes to HDX.
#'
#' This just drops a few columns, and
#' renames some for the final output data CSV that goes onto HDX. The data is
#' pushed first to the Azure blob store, and then triggers the HDX pipeline
#' https://github.com/OCHA-DAP/hdx-signals-alerts to push the data to HDX.
#'
#' The columns used are defined in `src/signals/template_data`.
#'
#' @param df Data frame to save out
save_core_signals_hdx <- function(df) {
  # use indicator mapping to filter out the core dataset
  # only those with `mc_interest` values are publicly subscribable
  df_hdx_ind <- cs$read_az_file("input/indicator_mapping.parquet") |>
    dplyr$filter(!is.na(mc_interest))

  # rename specific columns for use in HDX output
  df <- dplyr$rename(
    df,
    plot = plot_url,
    map = map_url,
    plot2 = plot2_url,
    other_images = other_images_urls,
    campaign_url = campaign_url_archive
  )

  cs$update_az_file(
    df = df[df$indicator_id %in% df_hdx_ind$indicator_id, names(template_data$signals_hdx_template)],
    name = "output/signals.csv"
  )

  push_hdx$push_hdx()
}
