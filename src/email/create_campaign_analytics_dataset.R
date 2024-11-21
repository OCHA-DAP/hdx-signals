#nolint start
FP_CAMPAIGN_DATA <- "output/user_research/hdx_signals_campaign_analytics_data.csv"
STORAGE_ACCOUNT <- "prod"
MC_SIGNALS_LIST_ID <- "e908cb9d48"
#nolint end

box::use(
  src / email / mailchimp / base_api,
  cs = src / utils / cloud_storage
)

box::use(
  dplyr,
  forcats,
  gg = ggplot2,
  gghdx,
  httr2,
  purrr,
  lubridate
)
RUN_DATE <- Sys.Date() #nolint

blob_detected <- cs$az_file_detect(
  pattern = FP_CAMPAIGN_DATA,
  container = STORAGE_ACCOUNT
)
dataset_on_blob <- ifelse(length(blob_detected) > 0, TRUE, FALSE)

# get all campaigns as a list
campaigns <- base_api$mc_api(
  lists_api = FALSE
) |>
  httr2$req_url_path_append(
    "campaigns"
  ) |>
  httr2$req_url_query(
    list_id = MC_SIGNALS_LIST_ID,
    count = 1000
  ) |>
  httr2$req_perform() |>
  httr2$resp_body_json()

# get dataframe of signals so we can filter out campaigns only to those sent
# and stored here

df_signals <- cs$read_az_file("output/signals.parquet")

# retrieve campaigns as a dataframe
df_all_campaigns <- purrr$map(
  .x = campaigns$campaigns,
  .f = \(x) {
    dplyr$tibble(
      id = x$id,
      date = lubridate$as_date(x$create_time),
      recipients = x$recipients$recipient_count,
      opens = x$report_summary$opens,
      unique_opens = x$report_summary$unique_opens,
      open_rate = x$report_summary$open_rate,
      clicks = x$report_summary$clicks,
      subscriber_clicks = x$report_summary$subscriber_clicks,
      click_rate = x$report_summary$click_rate
    )
  }
) |>
  purrr$list_rbind()

# gets campaign stats by filtering against things in df_signals
df_campaign_stats <- df_all_campaigns |>
  dplyr$filter(
    recipients > 1
  ) |>
  dplyr$left_join(
    y = df_signals |>
      dplyr$select(
        id = campaign_id_email,
        share_id = campaign_id_archive,
        iso3,
        indicator_id
      ),
    by = "id"
  ) |>
  dplyr$filter(
    !is.na(indicator_id) # filters out emails not in df_signals
  ) |>
  dplyr$mutate(
    date = lubridate$as_date(date),
    extraction_date = lubridate$as_date(RUN_DATE)
  )

if (!dataset_on_blob) {
  cs$update_az_file(
    df = df_campaign_stats,
    name = FP_CAMPAIGN_DATA,
    container = STORAGE_ACCOUNT
  )
}

if (dataset_on_blob) {
  df_campaign_prev <- cs$read_az_file(
    name = FP_CAMPAIGN_DATA,
    container = STORAGE_ACCOUNT
  )

  df_campaign_prev <- df_campaign_prev |>
    dplyr$mutate(
      date = lubridate$as_date(date),
      extraction_date = lubridate$as_date(extraction_date),
    )

  # return all rows from new data set that do not have matching rows in old data
  df_diff <- dplyr$anti_join(
    dplyr$select(df_campaign_stats, -extraction_date),
    dplyr$select(df_campaign_prev, -extraction_date)
  )

  # bind new records to previous records
  df_merged_long <- dplyr$bind_rows(
    df_campaign_prev,
    df_diff |>
      dplyr$mutate(
        extraction_date = lubridate$as_date(RUN_DATE)
      )
  )

  cs$update_az_file(
    df = df_merged_long,
    name = FP_CAMPAIGN_DATA,
    container = STORAGE_ACCOUNT
  )
}
