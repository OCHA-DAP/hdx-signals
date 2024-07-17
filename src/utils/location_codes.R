box::use(countrycode)
box::use(purrr)

box::use(cs = ./cloud_storage)

#' Get ISO3 code from location name
#'
#' @param location_names Vector of location names
#'
#' @export
names_to_iso3 <- function(location_names) {
  # these need custom handling because they don't match in `countryname()`
  custom_names <- list(
    Micronesia = "FSM",
    `US Outlying Minor Islands` = "UMI",
    Kosovo = "XKX",
    "Central Africa" = "CAF",
    "C. African Rep." = "CAF"
  )

  # separate out names that need custom handling
  iso3 <- character(length(location_names))
  custom_iso3 <- location_names %in% names(custom_names)
  iso3[custom_iso3] <- purrr$map_chr(location_names[custom_iso3], \(x) custom_names[[x]])
  iso3[!custom_iso3] <- countrycode$countryname(
    sourcevar = location_names[!custom_iso3],
    destination = "iso3c"
  )
  iso3
}

#' Get ISO3 code from ISO numeric codes
#'
#' @param ison Vector of ISO numeric codes
#'
#' @export
ison_to_iso3 <- function(ison) {
  iso3 <- character(length(ison))
  iso3[ison == 0] <- "XKX"
  iso3[ison != 0] <- countrycode$countrycode(
    sourcevar = ison[ison != 0],
    origin = "iso3n",
    destination = "iso3c"
  )
  iso3
}

#' Get ISOn code from ISO numeric codes
#'
#' @param iso3 Vector of ISO3 codes
#'
#' @export
iso3_to_ison <- function(iso3) {
  ison <- character(length(iso3))
  ison[iso3 == "XKX"] <- 0
  ison[iso3 != "XKX"] <- countrycode$countrycode(
    sourcevar = iso3[iso3 != "XKX"],
    origin = "iso3c",
    destination = "iso3n"
  )
  ison
}

#' Get ISO3 code from ISO2
#'
#' @param iso2 Vector of ISO2 codes
#'
#' @export
iso2_to_iso3 <- function(iso2) {
  iso3 <- character(length(iso2))
  custom_iso2 <- iso2 == "LAC"
  iso3[custom_iso2] <- "LAC" # IPC uses LAC as an ISO2 code
  iso3[!custom_iso2] <- countrycode$countrycode(
    sourcevar = iso2[!custom_iso2],
    origin = "iso2c",
    destination = "iso3c"
  )
  iso3
}

#' Get name from ISO3 code
#'
#' Get name from ISO3 codes. Uses the `locations_metadata.parquet` file to match them up.
#'
#' @export
iso3_to_names <- function(iso3) {
  df_metadata$location[match(iso3, df_metadata$iso3)]
}

#' Get region from ISO3 code
#'
#' Get region from ISO3 codes. Uses the `locations_metadata.parquet` file as the
#' definitive region coding.
#'
#' @export
iso3_to_regions <- function(iso3) {
  df_metadata$region[match(iso3, df_metadata$iso3)]
}

#' Get ISO3 code from ASAP0 ID
#'
#' Gets ISO3 code from ASAP0 id. Uses the `asap_iso3.parquet` file for coding.
#'
#' @export
asap_to_iso3 <- function(asap0_id) {
  df_asap$iso3[match(asap0_id, df_asap$asap0_id)]
}

df_metadata <- cs$read_az_file("input/locations_metadata.parquet")
df_asap <- cs$read_az_file("input/asap_iso3.parquet")
