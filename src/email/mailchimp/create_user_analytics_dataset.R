"
Create user analytics data set.

Script queries mailchimp API to get user analytics data and then combines it
into a tidy data.frame.

This will be run on a GitHub Action (.github/workflows/user_audience_analysis.yml)
Weekly.
"

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
  name =  "output/user_analytics/user_analytics_data.csv",
  container = "dev"
  )

