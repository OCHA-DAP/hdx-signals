box::use(glue)

box::use(../../utils/location_codes)
box::use(../../utils/formatters)
box::use(cs = ../../utils/cloud_storage)

#' Generate caption based on ISO3 code and dataset
#'
#' Takes in an `indicator_id` and `iso3` code, and returns a caption based on these
#' values. Indicator sourcing pulled from `input/indicator_mapping.parquet`. If
#' `map` is true, then adds in boundary information from
#' `input/locations_metadata.parquet`.
#'
#' @param indicator_id Indicator ID
#' @param iso3 ISO3 code
#' @param map Whether or not it's for a map. If `TRUE`, adds in boundary data
#'     sourcing.
#' @param extra_caption Extra caption to add as the first line.
#'
#' @export
caption <- function(indicator_id, iso3, map = FALSE, extra_caption = NULL) {
  ind_source <- df_ind$data_source[df_ind$indicator_id == indicator_id]
  if (map) {
    map_source <- paste0(
      "; Boundaries - ",
      df_metadata$boundary_source[df_metadata$iso3 == iso3]
    )
    map_caveat <- paste(
      "The boundaries and names shown and the designations used on this map",
      "do not imply official endorsement or acceptance by the United Nations.",
      sep = "\n"
    )
  } else {
    map_source <- ""
    map_caveat <- ""
  }

  if (is.null(extra_caption)) {
    extra_txt <- ""
  } else {
    extra_txt <- paste0(extra_caption, "\n")
  }

  glue$glue(
    "{extra_txt}",
    "Data source: {ind_source}{map_source}\n",
    "{location_codes$iso3_to_names(iso3)}, created {formatters$format_date(Sys.Date())}",
    "\n{map_caveat}"
  )
}

df_ind <- cs$read_az_file("input/indicator_mapping.parquet")
df_metadata <- cs$read_az_file("input/locations_metadata.parquet")
