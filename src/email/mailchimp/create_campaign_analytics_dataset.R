box::use(
  src/email/mailchimp/base_api,
  cs = src/utils/cloud_storage
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

# get all campaigns as a list
campaigns <- base_api$mc_api(
  lists_api = FALSE
) |>
  httr2$req_url_path_append(
    "campaigns"
  ) |>
  httr2$req_url_query(
    list_id = "e908cb9d48",
    count = 1000
  ) |>
  httr2$req_perform() |>
  httr2$resp_body_json()

# get dataframe of signals so we can filter out campaigns only to those sent and stored here
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
  )

cs$update_az_file(
  df = df_campaign_stats,
  name =  "output/user_analytics/campaing_analytics_data.csv",
  container = "dev"
)
