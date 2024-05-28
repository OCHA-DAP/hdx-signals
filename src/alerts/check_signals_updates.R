box::use(jsonlite)
box::use(httr)
box::use(purrr)
box::use(src/utils/get_env[get_env])
box::use(logger[log_error])
box::use(src/utils/hs_logger)
box::use(cs = src/utils/cloud_storage)

hs_logger$configure_logger()

org_name <- "ocha-dap"
repo_name <- "hdx-signals"
indicators_path <- "src/indicators"
indicators <- list.files(indicators_path)[file.info(file.path(indicators_path, list.files(indicators_path)))$isdir]
test <- Sys.getenv("TEST", unset = "FALSE")
test <- FALSE

process_run_status <- function(response, ind) {
  base_logs_url <- paste0("https://github.com/", org_name, "/", repo_name, "/actions/runs/")

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

gh_status <- function(ind, org_name, repo_name) {
  # TODO Temp cleaning before workflows are renamed to match indicator ids
  ind <- sub("^[^_]*_", "", ind)
  workflow_id <- paste0("monitor_", ind, ".yaml")
  token <- get_env("GH_TOKEN")
  url <- paste0("https://api.github.com/repos/", org_name, "/", repo_name, "/actions/workflows/", workflow_id, "/runs")
  response <- GET(url, add_headers(Authorization = paste("token", token)))
  return(response)
}

slack_post_message <- function(header, status_text, signals) {
  msg <- list(
    blocks = list(
      list(
        type = "section",
        text = list(
          type = "plain_text",
          text = header,
          emoji = TRUE
        )
      ),
      list(
        type = "section",
        text = list(
          type = "mrkdwn",
          text = signals
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

  json_body <- jsonlite$toJSON(msg, pretty = TRUE, auto_unbox = TRUE)

  response <- POST(get_env("SLACK_URL"), body = json_body, encode = "json")
  if (response$status != 200) {
    print(response)
    log_error("Error posting Slack message")
    stop()
  }
}

slack_build_header <- function(n_alerts) {
    if (n_alerts == 0) {
        title <- paste0(Sys.Date(), ": No signals identified")
    } else {
        title <- paste0(":rotating_light: <!channel> ", Sys.Date(), ": ", n_alerts, " alerts identified")
    }
    return(title)
}

slack_build_alert <- function(iso3, ind, campaign_url) {
  return(paste0(iso3, ": ", ind, " <", campaign_url, " | See draft campaign>\n"))
}


full_status <- ""
n_signals <- 0
signals <- ""
# TODO: Hard coding this because the IDMC names don't totally match the indicator_ids
indicators_azure <- c(
  "acled_conflict",
  "idmc_displacement_conflict",
  "idmc_displacement_disaster",
  "ipc_food_insecurity",
  "jrc_agricultural_hotspots"
)

for (ind in indicators_azure) {
  print(ind)
  fn_signals <- paste0(
    "output/",
    ind,
    if (test) "/test" else "",
    "/signals.parquet"
  )
  df <- cs$read_az_file(fn_signals)
  if(nrow(df) > 0) {
      for (i in 1:nrow(df)) {
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


for (ind in indicators) {
  gh_response <- gh_status(ind, org_name, repo_name)
  workflow_status <- process_run_status(gh_response, ind)
  full_status <- paste0(full_status, workflow_status)
}

header <- slack_build_header(n_signals)

print(n_signals)



slack_post_message(header, full_status, signals)
