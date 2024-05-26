library(httr)
library(jsonlite)
box::use(src/utils/get_env[get_env])
box::use(logger[log_error])
box::use(src/utils/hs_logger)
box::use(src/alerts/triage_signals[get_signals_df])

hs_logger$configure_logger()

org_name <- "ocha-dap"
repo_name <- "hdx-signals"
base_logs_url <- "https://github.com/OCHA-DAP/hdx-signals/actions/runs/"
indicators_path <- "src/indicators"
indicators <- list.files(indicators_path)[file.info(file.path(indicators_path, list.files(indicators_path)))$isdir]
test <- Sys.getenv("TEST", unset = "FALSE")

process_run_status <- function(response, ind, base_logs_url) {
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

    # Handle cases
    if (nrow(df_sel) == 1) {
      status <- df_sel[1, ]$workflow_runs.conclusion
      run_id <- df_sel[1, ]$workflow_runs.id
      if (status == "failure") {
        run_link <- paste0(base_logs_url, run_id)
        status_update <- paste0(":red_circle: ", ind, ": Failed update - <", run_link, "|Check logs> \n")
      } else if (status == "success") {
        status_update <- paste0(":large_green_circle: ", ind, ": Successful update \n")
      }
    } else if (nrow(df_sel) == 0) {
      status_update <- paste0(":heavy_minus_sign: ", ind, ": No scheduled update \n")
    } else {
      status_update <- paste0(":red_circle: ", ind, ": More than one scheduled run today \n")
    }
  } else {
    status_update <- paste0(":red_circle: ", ind, ": Failed request for workflow status - ", status_code(response), "\n")
  }

  return(status_update)
}

gh_status <- function(ind, org_name, repo_name) {
  # TODO Temp cleaning before workflows are renamed to match indicator ids
  ind <- sub("^[^_]*_", "", ind)
  workflow_id <- paste0("monitor_", ind, ".yaml")
  token <- get_env("GH_TOKEN")
  url <- paste0("https://api.github.com/repos/", org_name, "/", repo_name, "/actions/workflows/", workflow_id, "/runs")
  response <- GET(url, add_headers(Authorization = paste("token", token)))
  return(response)
}

post_slack_message <- function(status_text) {
  msg <- list(
    blocks = list(
      list(
        type = "section",
        text = list(
          type = "plain_text",
          text = paste0(Sys.Date(), ": No signals identified"),
          emoji = TRUE
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

full_status <- ""
for (ind in indicators) {
  print(ind)
  gh_response <- gh_status(ind, org_name, repo_name)
  workflow_status <- process_run_status(gh_response, ind, base_logs_url)
  full_status <- paste0(full_status, workflow_status)
}

post_slack_message(full_status)

for (ind in indicators) {
    fn_signals <- paste0(
        "output/",
        ind,
        if (test) "/test" else "",
        "/signals.parquet"
    )

    df <- get_signals_df(fn_signals)
}
