# create user analytics data set

box::use(
  src/email/mailchimp/audience,
  src/email/mailchimp/base_api,
  cs = src/utils/cloud_storage
)

box::use(
  dplyr,
  gg = ggplot2,
  gghdx,
  httr2,
  lubridate,
  purrr,
  scales,
  sf,
  stringr,
  tidyr
)


# script-specific custom functions ----------------------------------------


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


# Generate Data Set -------------------------------------------------------

member_list <- audience$mc_members()
# it appears that a user can select a topic (group) which can be a region
# or it can be dataset. They can then select the dataset they want or the
# country/countries within the region as an "interest"

# this is simply a lookup table with uid (interest_id) with all found combos!
df_interests <- audience$mc_groups() |>
  dplyr$select(
    interest_id,
    interest = name,
    group = title
  )

# this could be the dataset that we provide
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
      interests = dplyr$as_tibble(x$interests) # this is an interesting /non-conventional move... should run some experiments
    )
  }
) |>
  purrr$list_rbind() |>
  # ^ 1 row per user
  dplyr$mutate(
    email_count = purrr$map_dbl(
      .x = id,
      .f = mc_email_count
    )
  ) |>
  # 1 row per user her
  tidyr$unnest(interests) |> # still 1 row per user.
  # this is where it gets funkay...
  # currently all interests are wide w/ logical values per column
  # this pivots them longer creating multiple rows per user
  tidyr$pivot_longer(
    cols = dplyr$where(is.logical),
    names_to = "interest_id",
    values_to = "interested"
  ) |>
  # then this slaps a label on to the interest_id
  dplyr$left_join(
    y = df_interests,
    by = "interest_id"
  ) |>
  dplyr$filter(
    name != "" # HDX Signals email
  )


cs$update_az_file(
  df = df_members,
  name =  "user_data.parquet",
  container = "dev"
  )

# all scrap below

# # Here the data is summarised w/ some metrics deemed to be important
# df_members_distinct <- df_members |>
#   dplyr$group_by(
#     name, email, status, subscription_date, organisation, iso2, open_rate, click_rate, email_count, group
#   ) |>
#   # this sumarise basically gets us down unique groups based on user/email + group combo..
#   # so basically per group stats?
#   dplyr$summarize(
#     all_locations = any(interest == "All locations in the region" & interested),
#     interest_num = sum(interested[interest != "All locations in the region"]),
#     .groups = "drop_last"
#   ) |>
#   # then we summarise down to 1 row per user
#   dplyr$summarize(
#     # number of different data sets the user has selected as interested in
#     num_datasets = interest_num[group == "Datasets of interest"],
#     # did they select all datasets available (6)
#     all_datasets = num_datasets == 6,
#
#     num_all_locations = sum(all_locations),
#     all_locations = all(all_locations[group != "Datasets of interest"]),
#     everything = all_datasets && all_locations,
#     .groups = "drop"
#   )
#
# # what is open rate & click_rate
# df_members |>
#   gg$ggplot(
#     gg$aes(x= open_rate)
#   )+
#   gg$geom_histogram()
#
# df_members |>
#   dplyr$group_by(email,group) |>
#   dplyr$summarise(
#     click_rate = mean(click_rate),
#     .groups = "drop_last"
#   ) |>
#   dplyr$summarise(
#     click_rate_var = var(click_rate)
#   ) |>
#   dplyr$distinct(click_rate_var)
#
# df_members |>
#   gg$ggplot(
#     gg$aes(x= click_rate)
#   )+
#   gg$geom_histogram()
#
# df_members |>
#   dplyr$distinct(open_rate)
# df_members |>
#   tidyr$pivot_longer(cols = open_rate:email_count)
#
# df_members |>
#   dplyr$select(name, email, status,organisation,interested, interest,group, subscription_date) |>
#   dplyr$count(status)
#
# df_ck <- df_members |>
#
#   dplyr$group_by(
#     name,
#     email,
#     status,
#     subscription_date = lubridate$as_datetime(subscription_date),
#     organisation,
#     iso2,
#     open_rate,
#     click_rate,
#     email_count,
#     group
#
#   ) |>
#   # this sumarise basically gets us down unique groups based on user/email + group combo..
#   # so basically per group stats?
#   dplyr$summarize(
#     all_locations = any(interest == "All locations in the region" & interested),
#     interest_num = sum(interested[interest != "All locations in the region"]),
#     .groups = "drop_last"
#   ) |>
#   dplyr$group_by(
#     name,
#     email,
#     status,
#     subscription_date,
#     subscription_month = lubridate$as_date(lubridate$floor_date(subscription_date,"month")),
#     organisation,
#     iso2,
#     open_rate,
#     click_rate,
#     email_count
#
#
#   ) |>
#   # then we summarise down to 1 row per user
#   dplyr$summarize(
#     # number of different data sets the user has selected as interested in
#     num_datasets = interest_num[group == "Datasets of interest"],
#     # did they select all datasets available (6)
#     all_datasets = num_datasets == 6,
#
#     num_all_locations = sum(all_locations),
#     all_locations = all(all_locations[group != "Datasets of interest"]),
#     everything = all_datasets && all_locations,
#     .groups = "drop"
#   )
#
# df_ck |>
#   gg$ggplot(
#     gg$aes(x= num_datasets)
#   ) +
#   gg$geom_histogram()+
#   gg$facet_wrap(~subscription_month)
#   # dplyr$glimpse()
#   dplyr$group_by(subscription_month,)
