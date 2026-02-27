library(sf)

get_seasonal_warnings <- function(
    gaul_level,
    land_type = c("crop", "rangeland"),
    reference_date = Sys.Date()
    ) {
  if (!gaul_level %in% c(1, 2)) {
    stop("gaul_level must be 1 or 2")
    }
  land_type <- match.arg(land_type)
  reference_date <- as.character(reference_date)

  # ---- build layer name ----
  type_name <- paste0(
    "asap8:season_warning_gaul",
    gaul_level, "_",
    land_type, "_500k"
  )

  # ---- build URL ----
  url <- paste0(
    "https://agricultural-production-hotspots.ec.europa.eu/ows?",
    "service=WFS&",
    "version=2.0.0&",
    "request=GetFeature&",
    "typeNames=", type_name, "&",
    "outputFormat=application/json&",
    "viewparams=reference_date:", reference_date, "&",
    "propertyName=asap1_id,adm0_code,adm1_code,dekad,season_order,season_cnt"
  )

  # ---- read data ----
  st_read(url, quiet = TRUE)
}

df <- get_seasonal_warnings(1, "crop", "2023-12-11")
