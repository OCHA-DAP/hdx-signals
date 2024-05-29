box::use(jsonlite)
box::use(httr[GET, POST, add_headers, status_code, content])
box::use(httr2)
box::use(purrr)
box::use(logger)
box::use(stringr)

box::use(../utils/get_env[get_env])
box::use(../utils/hs_logger)
box::use(cs = ../utils/cloud_storage)

hs_logger$configure_logger()

#' Builds and posts a message to Slack, using incoming webhooks
#' See API docs: https://api.slack.com/messaging/webhooks
#'
#' @param header_text Text for the message header
#' @param status_text Text for the GitHub actions status report
#' @param signals_text Text for the signals status report
#'
#' @returns Nothing. Message is posted to slack and will log an error if not successful
slack_post_message <- function(header_text, status_text, signals_text) {
  # See https://app.slack.com/block-kit-builder for prototyping layouts in JSON
  msg <- list(
    blocks = list(
      list(
        type = "section",
        text = list(
          type = "plain_text",
          text = header_text,
          emoji = TRUE
        )
      ),
      list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = signals_text
        )
      ),
      list(type = "divider"),
      list(
        type = "context",
        elements = list(
          list(
            type = "mrkdwn",
            text = status_text
          )
        )
      ),
      list(type = "divider")
    )
  )

  # Create and perform the POST request using httr2
  response <- httr2$request(get_env("SLACK_URL")) |>
    httr2$req_body_json(msg) |>
    httr2$req_perform()

  if (response$status_code != 200) {
    logger$log_error("Error posting Slack message")
    stop()
  }
}

#' Builds the header text, depending on how many signals are reported
#'
#' @param n_signals Number of signals reported
#'
#' @returns String header text
slack_build_header <- function(n_signals) {
  if (n_signals == 0) {
    title <- paste0(Sys.Date(), ": No signals identified")
  } else {
    title <- paste0(":rotating_light: <!channel> ", Sys.Date(), ": ", n_signals, " alerts identified")
  }
  return(title)
}

#' Builds the signal alert text
#'
#' @param indicator_id Indicator ID
#' @param df DataFrame with all the signals for the given indicator
#'
#' @returns String signal alert text
slack_build_alert <- function(indicator_id, df) {
  paste0("*", indicator_id, "* - ", nrow(df), " countries impacted- <", df$campaign_url_archive[1], " | See draft campaign>\n")
}

#' Takes the response from a GitHub Actions run of a single indicator
#' and outputs a status message to be posted to Slack
#'
#' @param indicator_id ID of the indicator
#'
#' @returns String status message to be posted to Slack
slack_build_workflow_status <- function(indicator_id) {
  workflow_id <- paste0("monitor_", indicator_id, ".yaml")
  base_logs_url <- paste0("https://github.com/ocha-dap/hdx-signals/actions/runs/")

  df_runs <- tryCatch({
    httr2$request(
      "https://api.github.com/repos/ocha-dap/hdx-signals/actions/workflows"
    ) |>
    httr2$req_url_path_append(
      workflow_id,
      "runs"
    ) |>
    httr2$req_auth_bearer_token(
      token = get_env("GH_TOKEN")
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_string() |>
    jsonlite$fromJSON(flatten = TRUE) |>
    as.data.frame()
  },
  error = function(e) {
    logger$log_error(e$message)
    e$message
  })

  if (is.character(df_runs)) {
    return(paste0(
      ":red_circle: ",
      ind,
      ": Failed request for workflow status -",
      df_runs,
      "\n"
    ))
  }
  # Get today's scheduled runs from the main branch
  df_runs$date <- as.Date(df_runs$workflow_runs.created_at)
  df_sel <- subset(
    df_runs,
    workflow_runs.event == "schedule" &
      workflow_runs.head_branch == "main" &
      date == Sys.Date()
  )

  if (nrow(df_sel) == 1) {
    status <- df_sel[1, ]$workflow_runs.conclusion
    run_id <- df_sel[1, ]$workflow_runs.id
    if (status == "failure") {
      run_link <- paste0(base_logs_url, run_id)
      status_update <- paste0(":red_circle: ", indicator_id, ": Failed update - <", run_link, "|Check logs> \n")
    } else if (status == "success") {
      status_update <- paste0(":large_green_circle: ", indicator_id, ": Successful update \n")
    }
    # If no scheduled runs happened off of main today
  } else if (nrow(df_sel) == 0) {
    status_update <- paste0(":heavy_minus_sign: ", indicator_id, ": No scheduled update \n")
  } else {
    status_update <- paste0(":red_circle: ", indicator_id, ": More than one scheduled run today \n")
  }

  return(status_update)
}

# Get the indicator ids of all workflows
indicators_path <- ".github/workflows"
indicators_files <- list.files(indicators_path)
indicators_files <- indicators_files[stringr$str_starts(indicators_files, "monitor_")]
indicators <- stringr$str_extract(indicators_files, "(?<=monitor_)[^\\.]+(?=\\.yaml)")

full_status <- ""
n_signals <- 0
signals <- ""
test <- Sys.getenv("TEST", unset = TRUE)

for (ind in indicators) {
  logger$log_info(paste0("Checking GitHub Actions status for ", ind, "..."))
  workflow_status <- slack_build_workflow_status(ind)
  full_status <- paste0(full_status, workflow_status)
  logger$log_info("Successfully checked")

  logger$log_info(paste0("Checking for signals: ", ind, "..."))
  fn_signals <- paste0(
    "output/",
    ind,
    if (test) "/test" else "",
    "/signals.parquet"
  )
  df <- cs$read_az_file(fn_signals)
  if (nrow(df) > 0) {
    logger$log_info(paste0("Found signal for ", ind))
    alert <- slack_build_alert(ind, df)
    signals <- paste0(signals, alert)
    n_signals <- n_signals + 1
  }
}
logger$log_info(paste0("Found ", n_signals, " signals"))

header <- slack_build_header(n_signals)
slack_post_message(header, full_status, signals)
logger$log_info("Successfully posted message to Slack")
