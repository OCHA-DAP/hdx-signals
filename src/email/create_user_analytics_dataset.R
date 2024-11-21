"
Create user analytics data set.

Script queries mailchimp API to get user analytics data and then combines it
into a tidy data.frame.

This will be run on a GitHub Action (.github/workflows/user_audience_analysis.yml)
Weekly.
"

# putting this up here so that we can change to PROD later and only need
# to update here rather than multiple places.
FP_USER_INTERESTS = "output/user_research/hdx_signals_user_interests.csv"
FP_USER_INTERACTIONS = "output/user_research/hdx_signals_user_interactions.csv"
FP_USER_CONTACTS <- "output/user_research/hdx_signals_user_contacts.csv"

STORAGE_ACCOUNT <- "dev"

box::use(
  src/email/mailchimp/audience,
  src/email/mailchimp/base_api,
  src/email/mailchimp/audience_write_utils,
  cs = src/utils/cloud_storage,
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
  tidyr,
  AzureStor,
  logger
)

RUN_DATE <- Sys.Date()

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


df_members_new <- df_members |>
  dplyr$mutate(
    subscription_date = lubridate$as_date(subscription_date),
    iso2 = ifelse(iso2=="",NA_character_,iso2),
    extraction_date= lubridate$as_date(Sys.Date())
  ) |>
  dplyr$select(
    -dplyr$all_of(c("name","email"))
  )

df_member_interactions_new <- df_members_new |>
  dplyr$group_by(id) |>
  dplyr$distinct(open_rate,click_rate,email_count) |>
  dplyr$ungroup() |>
  dplyr$mutate(
    extraction_date = lubridate$as_date(Sys.Date())
  )


df_contact <-  df_members |>
  dplyr$distinct(id,name, email)

# we can just write this each time to keep active members
cs$update_az_file(
  df = df_contact,
  name =  FP_USER_CONTACTS,
  container = STORAGE_ACCOUNT
)

# these can be updated/apppended as time goes on
audience_write_utils$write_appended_data(
  df = df_member_interactions_new,
  file_path = FP_USER_INTERACTIONS,
  storage_account = STORAGE_ACCOUNT,
  run_date = RUN_DATE
  )

audience_write_utils$write_appended_data(
  df = df_members_new,
  file_path = FP_USER_INTERESTS,
  storage_account = STORAGE_ACCOUNT,
  run_date = RUN_DATE
  )
