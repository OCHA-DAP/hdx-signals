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

# general open rates

df_campaign_stats |>
  dplyr$mutate(
    indicator_id = forcats$fct_reorder(
      .f = factor(indicator_id),
      .x = open_rate
    )
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = open_rate,
      y = indicator_id
    )
  ) +
  gg$geom_boxplot(
    fill = "white",
    outliers = FALSE
  ) +
  gg$geom_point(
    alpha = 0.2
  ) +
  gg$scale_x_continuous(
    labels = scales$label_percent()
  ) +
  gg$labs(
    x = "Open rate",
    y = "",
    title = "Open rate by indicator"
  )

# open rate by time

df_campaign_stats |>
  gg$ggplot(
    mapping = gg$aes(
      x = date,
      y = open_rate
    )
  ) +
  gg$geom_point() +
  gg$labs(
    x = "",
    y = "Open rate",
    title = "Open rate across time"
  ) +
  gg$scale_y_continuous(
    labels = scales$label_percent(),
    limits = c(.05, NA)
  )

# now look at open rate vs email count
df_campaign_stats |>
  dplyr$left_join(
    dplyr$select(
      df_all_campaigns,
      share_id = id,
      share_clicks = clicks,
      share_opens = opens,
      share_unique_opens = unique_opens
    )
  )
