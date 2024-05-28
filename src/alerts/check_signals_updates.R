box::use(jsonlite[fromJSON, toJSON])
box::use(httr[GET, POST, add_headers, status_code, content])
box::use(purrr)
box::use(logger[log_error, log_info])

box::use(../utils/get_env[get_env])
box::use(../utils/hs_logger)
box::use(cs = ../utils/cloud_storage)

hs_logger$configure_logger()


#' Queries the GitHub API to get all runs from a given monitoring workflow
#' Requires workflows to be named following this convention: `monitor_<indicator_id>.yaml`
#'
#' NOTE: More data will be returned as each workflow is run more times. Perhaps won't scale well,
#' depending on how many runs a workflow will have
#'
#' @param indicator_id ID of the indicator
#'
#' @returns JSON API response
gh_status <- function(indicator_id) {
  # TODO Temp cleaning before workflows are renamed to match indicator ids
  ind <- sub("^[^_]*_", "", indicator_id)
  workflow_id <- paste0("monitor_", ind, ".yaml")
  token <- get_env("GH_TOKEN")
  url <- paste0("https://api.github.com/repos/ocha-dap/hdx-signals/actions/workflows/", workflow_id, "/runs")
  response <- GET(url, add_headers(Authorization = paste("token", token)))
  return(response)
}

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
  json_body <- toJSON(msg, pretty = TRUE, auto_unbox = TRUE)
  response <- POST(get_env("SLACK_URL"), body = json_body, encode = "json")
  if (response$status != 200) {
    log_error("Error posting Slack message")
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
#' @param iso3 Country iso3 code
#' @param indicator_id Indicator ID
#' @param campaign_url URL of Mailchimp campaign to review
#'
#' @returns String signal alert text
slack_build_alert <- function(iso3, indicator_id, campaign_url) {
  return(paste0(iso3, ": ", indicator_id, " <", campaign_url, " | See draft campaign>\n"))
}

#' Takes the response from a GitHub Actions run of a single indicator
#' and outputs a status message to be posted to Slack
#'
#' @param response JSON payload returned from `gh_status`
#' @param indicator_id ID of the indicator
#'
#' @returns String status message to be posted to Slack
slack_build_workflow_status <- function(response, indicator_id) {
  base_logs_url <- paste0("https://github.com/ocha-dap/hdx-signals/actions/runs/")

  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content, flatten = TRUE)
    df_runs <- as.data.frame(json_data)
    df_runs$date <- as.Date(df_runs$workflow_runs.created_at)

    # Get today's scheduled runs from the main branch
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
  } else {
    status_update <- paste0(
      ":red_circle: ",
      ind,
      ": Failed request for workflow status - ",
      status_code(response),
      "\n"
    )
  }

  return(status_update)
}

indicators_path <- "src/indicators"
indicators <- list.files(indicators_path)[file.info(file.path(indicators_path, list.files(indicators_path)))$isdir]
full_status <- ""
n_signals <- 0
signals <- ""

# TODO: Hard coding this because the monitoring scripts don't totally match the indictor ids
indicators_azure <- c(
  "acled_conflict",
  "idmc_displacement_conflict",
  "idmc_displacement_disaster",
  "ipc_food_insecurity",
  "jrc_agricultural_hotspots"
)

log_info("Checking GitHub Actions status...")
for (ind in indicators) {
  gh_response <- gh_status(ind)
  workflow_status <- slack_build_workflow_status(gh_response, ind)
  full_status <- paste0(full_status, workflow_status)
}
log_info("Successfully checked")

log_info("Checking for signals across all indicators...")
for (ind in indicators_azure) {
  test <- Sys.getenv("TEST", unset = TRUE)
  fn_signals <- paste0(
    "output/",
    ind,
    if (test) "/test" else "",
    "/signals.parquet"
  )
  df <- cs$read_az_file(fn_signals)
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      alert <- slack_build_alert(
        row["iso3"],
        row["indicator_name"],
        row["campaign_url_email"]
      )
      signals <- paste0(signals, alert)
    }
    n_signals <- n_signals + nrow(df)
  }
}
log_info(paste0("Found ", n_signals, " signals"))

header <- slack_build_header(n_signals)
slack_post_message(header, full_status, signals)
log_info("Successfully posted message to Slack")
