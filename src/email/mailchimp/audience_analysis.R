box::use(
  src/email/mailchimp/audience,
  src/email/mailchimp/base_api
)

box::use(
  dplyr,
  gg = ggplot2,
  gghdx,
  httr2,
  lubridate,
  purrr,
  rnaturalearth,
  scales,
  sf,
  stringr,
  tidyr
)

member_list <- audience$mc_members()
df_interests <- audience$mc_groups() |>
  dplyr$select(
    interest_id,
    interest = name,
    group = title
  )

#' Get number of emails sent to subscriber
#'
#' Provided a `member_id` (subscriber_hash in the Mailchimp documentation sometimes),
#' we pull in the number of mailings they have been sent.
#'
#' @export
mc_email_count <- function(member_id) {
  email_count <- mc_email_count_req(member_id = member_id)

  offset <- 1000
  while (email_count == offset) {
    email_count <- email_count + mc_email_count_req(member_id = member_id, offset = offset)
    offset <- offset + 1000
  }
  email_count
}

#' Repeatable call to use offset
mc_email_count_req <- function(member_id, offset = 0) {
  base_api$mc_api() |>
    httr2$req_url_path_append(
      "members",
      member_id,
      "activity-feed"
    ) |>
    httr2$req_url_query(
      count = 1000,
      offset = offset,
      activity_filters = "sent"
    ) |>
    httr2$req_perform() |>
    httr2$resp_body_json() |>
    purrr$pluck("activity") |>
    length()
}


df_members <- purrr$map(
  .x = member_list,
  .f = \(x) {
    dplyr$tibble(
      name = x$full_name,
      email = x$email_address,
      id = x$id,
      subscription_date = x$timestamp_opt,
      organisation = x$merge_fields$ORG,
      iso2 = x$location$country_code,
      status = x$status,
      open_rate = x$stats$avg_open_rate,
      click_rate = x$stats$avg_click_rate,
      interests = dplyr$as_tibble(x$interests)
    )
  }
) |>
  purrr$list_rbind() |>
  dplyr$mutate(
    email_count = purrr$map_dbl(
      .x = id,
      .f = mc_email_count
    )
  ) |>
  tidyr$unnest(interests) |>
  tidyr$pivot_longer(
    cols = dplyr$where(is.logical),
    names_to = "interest_id",
    values_to = "interested"
  ) |>
  dplyr$left_join(
    y = df_interests,
    by = "interest_id"
  ) |>
  dplyr$filter(
    name != "" # HDX Signals email
  )

# audience without interest analysis
df_members_distinct <- df_members |>
  dplyr$group_by(
    name, email, status, subscription_date, organisation, iso2, open_rate, click_rate, email_count, group
  ) |>
  dplyr$summarize(
    all_locations = any(interest == "All locations in the region" & interested),
    interest_num = sum(interested[interest != "All locations in the region"]),
    .groups = "drop_last"
  ) |>
  dplyr$summarize(
    num_datasets = interest_num[group == "Datasets of interest"],
    all_datasets = num_datasets == 6,
    num_all_locations = sum(all_locations),
    all_locations = all(all_locations[group != "Datasets of interest"]),
    everything = all_datasets && all_locations,
    .groups = "drop"
  )

###################################
#### ANALYSIS FOR PRESENTATION ####
###################################

gghdx$gghdx()

df_subscribed <- df_members_distinct |>
  dplyr$filter(
    status == "subscribed"
  )

# number of subscribers
nrow(df_subscribed)

# subscribers by organisation
df_subscribed |>
  dplyr$filter(
    status == "subscribed"
  ) |>
  dplyr$mutate(
    organisation = dplyr$case_when(
      stringr$str_detect(organisation, "OCHA") ~ "OCHA",
      organisation == "German Federal Foreign Office" ~ "GFFO",
      organisation %in% c("Centre for Humanitarian Data", "HDX") ~ "CHD/HDX",
      organisation == "USAID" ~ "USAID/BHA",
      organisation == "Impact Initiatives" ~ "IMPACT Initiatives",
      organisation %in% c("International Rescue Committee", "IRC International Rescue Committee") ~ "IRC",
      organisation == "World Health Organization" ~ "WHO",
      organisation %in% c("Iom", "International Organisation for Migration") ~ "IOM",
      organisation %in% c("Complex Risk Analytics Fund (CRAF’d)", "CRAF'd | UN EOSG") ~ "CRAF'd",
      TRUE ~ organisation
    )
  ) |>
  dplyr$count(
    organisation
  ) |>
  dplyr$arrange(
    n
  ) |>
  dplyr$mutate(
    organisation = factor(organisation, levels = organisation)
  ) |>
  dplyr$slice_tail(
    n = 20
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = n,
      y = organisation
    )
  ) +
  gg$geom_bar(
    stat = "identity"
  ) +
  gghdx$geom_text_hdx(
    mapping = gg$aes(
      label = n
    ),
    nudge_x = 1.5,
    fontface = "bold",
    hjust = 0
  ) +
  gg$labs(
    x = "# of subscribers",
    y = "",
    title = "Top 20 subscribing organisations"
  )

# map of the data
df_subscribed |>
  dplyr$count(
    iso2
  ) |>
  dplyr$filter(
    iso2 != ""
  ) |>
  dplyr$right_join(
    y = rnaturalearth$countries110,
    by = c("iso2" = "ISO_A2")
  ) |>
  dplyr$filter(
    ISO_A3 != "ATA"
  ) |>
  sf$st_as_sf() |>
  gg$ggplot(
    mapping = gg$aes(
      fill = n,
      label = n
    )
  ) +
  gg$geom_sf() +
  gg$geom_sf_text(
    color = "black"
  ) +
  gg$scale_fill_gradient(
    low = gghdx$hdx_hex("mint-ultra-light"),
    high = gghdx$hdx_hex("mint-dark"),
    na.value = "white"
  ) +
  gg$theme(
    legend.position = "none",
    panel.grid = gg$element_blank(),
    axis.line = gg$element_blank(),
    axis.text = gg$element_blank()
  ) +
  gg$labs(
    x = "",
    y = "",
    title = "Location of subscribers"
  )

# how many people are subscribing to everything
df_subscribed |>
  dplyr$summarize(
    everything = sum(everything) / dplyr$n(),
    all_locations = sum(all_locations) / dplyr$n(),
    all_datasets = sum(all_datasets) / dplyr$n()
  ) |>
  tidyr$pivot_longer(
    cols = tidyr$everything()
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = value,
      y = name
    )
  ) +
  gg$geom_bar(
    stat = "identity"
  ) +
  gg$scale_y_discrete(
    labels = c("All datasets", "All locations", "Everything")
  ) +
  gg$scale_x_continuous(
    labels = scales$label_percent()
  ) +
  gg$labs(
    x = "",
    y = "",
    title = "Bulk subscriptions"
  )

# analyze dataset subscriptions
df_members |>
  dplyr$filter(
    group == "Datasets of interest",
    status == "subscribed"
  ) |>
  dplyr$group_by(
    interest
  ) |>
  dplyr$summarize(
    n = sum(interested) / length(unique(id))
  ) |>
  dplyr$arrange(
    n
  ) |>
  dplyr$mutate(
    interest = factor(interest, levels = interest)
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = n,
      y = interest
    )
  ) +
  gg$geom_bar(
    stat = "identity"
  ) +
  gg$scale_x_continuous(
    labels = scales$label_percent()
  ) +
  gg$labs(
    x = "",
    y = "",
    title = "Most popular datasets"
  )

# number of datasets

df_members |>
  dplyr$filter(
    group == "Datasets of interest"
  ) |>
  dplyr$group_by(
    id
  ) |>
  dplyr$filter(
    !all(interested)
  ) |>
  dplyr$group_by(
    interest
  ) |>
  dplyr$summarize(
    n = sum(interested) / length(unique(id)),
    .groups = "drop"
  )

# locations analysis
df_subscribed |>
  dplyr$count(
    num_all_locations
  ) |>
  dplyr$mutate(
    pct = n / sum(n)
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = pct,
      y = as.factor(num_all_locations)
    )
  ) +
  gg$geom_bar(
    stat = "identity"
  ) +
  gg$labs(
    x = "",
    y = "# of regions subscribed to",
    title = "Subscriptions to entire regions"
  ) +
  gg$scale_x_continuous(
    labels = scales$label_percent()
  )

# What regions are most popular

df_members |>
  dplyr$filter(
    status == "subscribed",
    interest == "All locations in the region"
  ) |>
  dplyr$group_by(
    group
  ) |>
  dplyr$summarize(
    n = sum(interested) / length(unique(id))
  ) |>
  dplyr$arrange(
    n
  ) |>
  dplyr$mutate(
    group = factor(group, levels = group)
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = n,
      y = group
    )
  ) +
  gg$geom_bar(
    stat = "identity"
  ) +
  gg$scale_x_continuous(
    labels = scales$label_percent()
  ) +
  gg$labs(
    x = "",
    y = "",
    title = "Most popular regions"
  )

# most popular HRP locations

df_members |>
  dplyr$filter(
    status == "subscribed",
    interest != "All locations in the region",
    group != "Datasets of interest"
  ) |>
  dplyr$group_by(
    interest
  ) |>
  dplyr$summarize(
    n = sum(interested) / length(unique(id))
  ) |>
  dplyr$arrange(
    n
  ) |>
  dplyr$mutate(
    interest = factor(interest, levels = interest)
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = n,
      y = interest
    )
  ) +
  gg$geom_bar(
    stat = "identity"
  ) +
  gg$scale_x_continuous(
    labels = scales$label_percent()
  ) +
  gg$labs(
    x = "",
    y = "",
    title = "Most popular HRP locations"
  )

# general analysis statistics

df_subscribed$open_rate |> mean()
df_subscribed$click_rate |> mean()

# subscription timeline

df_subscribed |>
  dplyr$mutate(
    subscription_date = lubridate$as_date(subscription_date)
  ) |>
  tidyr$complete(
    subscription_date = seq.Date(from = min(subscription_date), to = max(subscription_date), by = "day"),
  ) |>
  dplyr$group_by(
    subscription_date
  ) |>
  dplyr$summarize(
    subscriptions = sum(!is.na(name)),
    .groups = "drop"
  ) |>
  dplyr$mutate(
    subscriptions_cumsum = cumsum(subscriptions)
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = subscription_date,
      y = subscriptions_cumsum
    )
  ) +
  gg$geom_line(
    linewidth = 1
  ) +
  gghdx$scale_y_continuous_hdx() +
  gg$labs(
    x = "",
    y = "# of subscribers",
    title = "Subscription timeline"
  )

# number of emails received

df_subscribed |>
  dplyr$count(
    email_count
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      x = n / sum(n),
      y = as.factor(email_count)
    )
  ) +
  gg$geom_bar(
    stat = "identity"
  ) +
  gg$theme(
    panel.grid = gg$element_blank()
  ) +
  gg$scale_x_continuous(
    labels = scales$label_percent()
  ) +
  gg$labs(
    x = "",
    y = "# of emails received",
    title = "# of emails received by subscribers"
  )

# subscription date and emails received
df_subscribed |>
  gg$ggplot(
    mapping = gg$aes(
      x = lubridate$as_date(subscription_date),
      y = email_count
    )
  ) +
  gg$geom_point() +
  gghdx$scale_y_continuous_hdx() +
  gg$coord_cartesian(
    clip = "off"
  ) +
  gg$labs(
    x = "",
    y = "# of emails received",
    title = "Emails received relative to subscription date"
  )

# open and click rates relative to emails

df_subscribed |>
  dplyr$mutate(
    groups = cut(
      x = email_count,
      breaks = seq(0, 35, 5),
      labels = paste(
        seq(0, 30, 5) + 1,
        seq(5, 35, 5),
        sep = "-"
      ),
      include.lowest = FALSE
    )
  ) |>
  dplyr$filter(
    !is.na(groups)
  ) |>
  gg$ggplot(
    mapping = gg$aes(
      y = open_rate,
      x = groups,
      group = groups
    )
  ) +
  gg$geom_boxplot(
    fill = "white"
  ) +
  gg$geom_point(
    alpha = 0.2
  ) +
  gg$scale_y_continuous(
    labels = scales$label_percent()
  ) +
  gg$labs(
    x = "# of emails received",
    y = "Open rate",
    title = "Open rate compared to receipt of emails"
  )

df_subscribed |>
  dplyr$arrange(
    dplyr$desc(
      open_rate
    )
  ) |>
  dplyr$filter(
    email_count > 10,
    !(organisation %in% c("HDX", "CHD", "Centre for Humanitarian Data")),
    !(name %in% c("Troy", "test test", "Isabelle Tot", "Tristan Downing"))
  ) |>
  dplyr$slice_head(
    n = 20
  ) |>
  dplyr$select(
    name, organisation, open_rate, click_rate, email_count
  )
