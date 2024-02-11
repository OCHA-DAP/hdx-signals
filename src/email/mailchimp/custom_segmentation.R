box::use(dplyr)
box::use(rlang[`!!`, arg_match])

# local modules
box::use(./base_api)
box::use(./segments)
box::use(./audience)
box::use(./tags)
box::use(cs = ../../utils/cloud_storage)

#' Generate email segmentation
#'
#' Generates the email segment to send with the specific campaign. Produces
#' two conditions lists, one based on the countries which includes all relevant
#' regions receiving emails. The other is based on signal level and indicator.
#'
#' @param indicator_id Unique indicator ID
#' @param iso3 Vector of ISO3 codes to generate conditions for
#' @param alert_level Level to alert from, either "Medium concern" or "High concern".
#'
#'
#' @returns List to be used as `recipients` to add or update a campaign.
#'
#' @export
mc_segment_conditions <- function(indicator_id, iso3, alert_level) {
  indicator_conditions <- mc_indicator_conditions(indicator_id, alert_level)
  iso3_conditions <- mc_iso3_conditions(iso3)

  list(
    list_id = "e908cb9d48",
    segment_opts = list(
      match = "all",
      conditions = list(
        indicator_conditions,
        iso3_conditions
      )
    )
  )
}

#' Returns the empty segment ID
#'
#' Checks that the empty segment is still empty, and updates if not.
#'
#' @returns Segment conditions for `hdx-signals-empty`, which is always ID 24989.
#'
#' @export
mc_empty_segment <- function() {
  # always ensure the segment is empty
  if (segments$mc_segment_member_count(24989) != 0) {
    segments$mc_update_static_segment(
      segment_id = 24989,
      segment_name = "hdx-signals-empty",
      emails = list()
    )
  }

  list(
    list_id = "e908cb9d48",
    segment_opts = list(
      saved_segment_id = 24989
    )
  )
}

#' Create Mailchimps conditions for ISO3 codes
#'
#' Converts ISO3 codes into a list of conditions, using
#' `mc_iso3_interests()` to generate the IDs to include in the email.
#'
#' @params iso3 Vector of ISO3 codes
#'
#' @returns Conditions list
mc_iso3_conditions <- function(iso3) {
  segments$mc_group_conditions(
    category_id = "22b9c25441",
    segment_ids = audience$mc_interests_iso3(iso3, use = "segmentation")
  )
}

#' Get conditions for a specific indicator
#'
#' For a specific indicator, return the relevant conditions for the email. For
#' publicly subscribable groups, this produces a segment based on the `alert_level`
#' and the merge field. If it is a private indicator only mailed it to tags, a
#' tag conditions list is returned.
#'
#' The mapping of `indicator_id` to either Mailchimp field or tags is contained
#' in the dataset `input/indicator_mapping.parquet`.
#'
#' @param indicator_id Unique identifier for the indicator
#' @param alert_level To what level email should be sent
mc_indicator_conditions <- function(indicator_id, alert_level) {
  df_ind <- cs$read_gcs_file("input/indicator_mapping.parquet") |>
    dplyr$filter(
      indicator_id == !!indicator_id
    )

  if (!is.na(df_ind$mc_field)) {
    mc_field_conditions(
      field = df_ind$mc_field,
      option = alert_level
    )
  } else if (!is.na(df_ind$mc_tag)) {
    mc_tag_conditions(df_ind$mc_tag)
  }
}

#' Create Mailchimp conditions for indicators
#'
#' For each indicator, creates the conditions necessary to segment by. This only
#' works for public indicators that are available for signup on the form. The
#' available options should be"Medium concern" or "High concern".
#'
#' @param field Name of the field
#' @param option Option to segment with, either
#'
#' @returns Conditions list
mc_field_conditions <- function(field, option = c("Medium concern", "High concern")) {
  option <- arg_match(option)
  segments$mc_merge_conditions(
    field_id = audience$mc_fields_ids(field),
    value_string = option,
    op = "is"
  )
}

#' Create segment conditions for tag
#'
#' For a specific tag name, we create the segment conditions for that tag.
#' We first check to see if we have a static segment for that tag, and if we do,
#' update it, otherwise create it. Then we create the segment conditions to use
#' that segment.
#'
#' @param tag_name Name of the tag
#'
#' @returns Conditions list
mc_tag_conditions <- function(tag_name) {
  segment_name <- paste0("private-segment-", tag_name)
  emails <- tags$mc_tag_members(tag_name)

  # update or create static segment
  segment_id <- segments$mc_find_segment(segment_name)
  if (is.null(segment_id)) { # create static segment if it doesn't exist
    segment_id <- segments$mc_add_static_segment(
      segment_name = segment_name,
      emails = emails
    )
  } else { # update the static segment to ensure all emails in the segment
    segments$mc_update_static_segment(
      segment_id = segment_id,
      segment_name = segment_name,
      emails = emails
    )
  }
  segments$mc_static_conditions(segment_id)
}
