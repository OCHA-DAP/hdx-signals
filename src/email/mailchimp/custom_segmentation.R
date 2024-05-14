box::use(dplyr)
box::use(purrr)
box::use(rlang[`!!`])

# local modules
box::use(./segments)
box::use(./audience)
box::use(cs = ../../utils/cloud_storage)
box::use(../../utils/country_codes)

#' Generate email segmentation
#'
#' Generates the email segment to send with the specific campaign. There are
#' limitations in the Mailchimp API that prevents us from segmenting by complex
#' conditions. Since it only allows joining segment conditions with "any" or "all",
#' it is not able to capture our use case. This is because our interest groups
#' (the indicators and regions/countries) are stored as separate categories
#' in Mailchimp, so subsetting the audience by indicators and each region requires
#' a separate segment condition. So, segmenting with the "any" condition doesn't
#' work because that doesn't require the user subscribed to that indicator, it
#' just checks that it meets any of the region/indicator conditions. And using
#' "all" doesn't work because if we are trying to send for multiple regions, it
#' would only send to users subscribed to every region we are sending to.
#'
#' Thus, we need to create custom static segments (lists of emails to send to) by
#' filtering our audience manually. This function creates a manual segment for
#' the `indicator_id` and countries covered in the alert. The static segment is
#' saved with the `indicator_id` as its name, and overwritten every team, because
#' we do not need to maintain this segmentation in the long run.
#'
#' @param indicator_id Unique indicator ID
#' @param iso3 Vector of ISO3 codes to generate conditions for
#' @param test Whether or not this is a test email. If so, only sends to contacts
#'     with the `hdx-signals-test` tag in Mailchimp.
#'
#' @returns Static segment ID from mailchimp.
#'
#' @export
mc_email_segment <- function(indicator_id, iso3, test = FALSE) {
  df_ind <- dplyr$filter(df_ind, indicator_id == !!indicator_id)
  emails <- mc_subscriber_emails(
    df_ind = df_ind,
    iso3 = iso3,
    test = test
  )

  segments$mc_update_static_segment(
    segment_id = df_ind$static_segment,
    segment_name = indicator_id,
    emails = as.list(emails)
  )

  list(
    list_id = "e908cb9d48",
    segment_opts = list(
      saved_segment_id = df_ind$static_segment
    )
  )
}

#' Filter member emails for segmentation
#'
#' Filters out members from the full Mailchimp registration, and gets their
#' emails if they have subscribed to receive signals about a specific indicator
#' or country. The `df_ind` passed in must already be filtered to a specific
#' indicator ID.
mc_subscriber_emails <- function(df_ind, iso3, test) {
  # first we get the list of interest ids based on the iso3 codes
  regions <- unique(country_codes$iso3_to_regions(iso3))
  countries <- country_codes$iso3_to_names(iso3)
  # get the regional subscription IDs
  region_ids <- df_interests |>
    dplyr$filter(
      title %in% regions,
      name == "All countries in the region"
    ) |>
    dplyr$pull(
      interest_id
    )

  # find any IDs for subscribing to the specific country
  country_ids <- df_interests$interest_id[match(countries, df_interests$name, nomatch = 0)]
  geo_ids <- c(region_ids, country_ids)

  # filter members to the specific indicator, by interest if a public subscription
  # otherwise we filter by manual tagging for indicators that are privately added
  if (!is.na(df_ind$mc_interest)) {
    member_emails <- interest_emails(interest = df_ind$mc_interest, geo_ids = geo_ids, test = test)
  } else {
    # keeps members tagged with this indicator
    member_emails <- tag_emails(interest_tag = df_ind$mc_tag, geo_ids = geo_ids, test = test)
  }
  member_emails[!is.na(member_emails)]
}

#' Returns emails for specific indicator interest
#'
#' Gets emails for interest and geo_ids.
interest_emails <- function(interest, geo_ids, test) {
  interest_id <- df_interests$interest_id[match(interest, df_interests$name)]
  purrr$map_chr(
    .x = member_list,
    .f = \(member) {
      ind_interest <- member$interests[[interest_id]]
      if (ind_interest && (!test || "hdx-signals-test" %in% purrr$map_chr(member$tags, \(tag) tag$name))) {
        # only check for countries if they were interested in the indicator
        # returns email if they were signed up to any of the geographies signalled
        # otherwise it returns an empty character vector
        if (any(as.logical(member$interests[geo_ids]))) {
          return(
            member$email_address
          )
        }
      }
      return(
        NA_character_
      )
    }
  )
}

#' Returns emails for indicator tags
#'
#' Gets emails for indicator tags based on the tag, geo_ids, and test
tag_emails <- function(interest_tag, geo_ids, test) {
  purrr$map_chr(
    .x = member_list,
    .f = \(member) {
      member_tags <- purrr$map_chr(member$tags, \(tag) tag$name)
      tag_interest <- tag_interest %in% member_tags
      if (tag_interest && (!test || "hdx-signals-test" %in% member_tags)) {
        # only check for countries if they were interested in the indicator
        # returns email if they were signed up to any of the geographies signalled
        # otherwise it returns an empty character vector
        if (any(as.logical(member$interests[geo_ids]))) {
          return(
            member$email_address
          )
        }
      }
      return(
        NA_character_
      )
    }
  )
}

#' Returns the archive segment ID
#'
#' Checks that the archive segment has just one member, and updates it if not.
#' Used to send emails for the archive simply back to the HDX Signals email
#' address, not to any external recipients.
#'
#' @returns Segment conditions for `hdx-signals-archive`, which is always ID 24989.
#'
#' @export
mc_archive_segment <- function() {
  # always ensure the segment just has one member
  if (segments$mc_segment_member_count(25085) != 1) {
    segments$mc_update_static_segment(
      segment_id = 25085,
      segment_name = "hdx-signals-archive",
      emails = list(Sys.getenv("HDX_SIGNALS_EMAIL"))
    )
  }

  list(
    list_id = "e908cb9d48",
    segment_opts = list(
      saved_segment_id = 25085
    )
  )
}

df_interests <- audience$mc_groups()
member_list <- audience$mc_members()
df_ind <- cs$read_az_file("input/indicator_mapping.parquet")
