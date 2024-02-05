box::use(countrycode)

# local modules
box::use(./base_api)
box::use(./segments)
box::use(./audience)
box::use(cs = ../../utils/cloud_storage)

#' Get Mailchimp interests for ISO3 codes
#'
#' Return all relevant Mailchimp interests for the ISO3 codes. The interests are UNHCR
#' regions and whether or not they are an HRP country.
#'
#' For the list of ISO3 codes, all relevant interests are returned that cover the
#' ISO3 codes. So if AFG and GBR are passed in, the European and Asian and the
#' Pacific regions would be returned, as well as HRP countries. This is to
#' ensure that email segmentation is sent to everyone interested in the
#' list of countries provided. By default, the IDs are returned, which can
#' be used for segmentation using `mc_group_conditions()`. Otherwise, full names
#' are returned which are used in conditional blocks.
#'
#' @param iso3 Vector of ISO3 codes
#' @param ids Logical, if `TRUE`, interest IDs returned, otherwise full names
#'     are returned.
#'
#' @returns Interest names string separated by commas or list of interest IDs
#'
#' @export
mc_iso3_interests <- function(iso3, ids = TRUE) {
  regions <- countrycode$countrycode(iso3, origin = "iso3c", destination = "unhcr.region")
  hrp_countries <- cs$read_gcs_file("input/hrp_countries.parquet")

  if (any(iso3 %in% hrp_countries$iso3)) {
    regions <- c(regions, "HRP countries")
  }

  regions <- sort(unique(regions))

  if (ids) {
    audience$mc_interest_ids(regions)
  } else {
    paste(regions, collapse = ",")
  }
}
