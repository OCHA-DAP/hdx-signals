#' Simple script to manually create map dimensions and settings
#'
#' Allows us to manually set all map dimensions for ISO3 codes so they look good
#' when saved. Aim is to have plot somewhere around 3 inches minimum, 5 inches
#' maximum. Currently only working on `height` and `width`.

box::use(dplyr)
box::use(purrr)
box::use(tidyr)
box::use(sf)

box::use(../src/utils/get_iso3_sf)
box::use(../src/utils/all_iso3_codes)
box::use(cs = ../src/utils/cloud_storage)

#' Gets width to height ratio for base adm0 of iso3
iso3_map_ratio <- function(iso3) {
  sf_obj <- get_iso3_sf$get_iso3_sf(iso3)
  bbox <- sf$st_bbox(sf_obj)
  (bbox[["xmax"]] - bbox[["xmin"]]) / (bbox[["ymax"]] - bbox[["ymin"]])
}

#' Gets the width and height of the map from the map ratio
#'
#' Just produces plots with equal area. Will likely need adjustments for specific
#' countries.
iso3_width_height <- function(iso3) {
  ratio <- iso3_map_ratio(iso3)
  height <- max(min(sqrt(24 / ratio), 8), 3) # ensure the ratios are bound to be 8x3 as most lopsided
  width <- 24 / height
  data.frame(width, height)
}

# get the widths and heights for all countries
iso3_codes <- all_iso3_codes$all_iso3_codes()

df_width_height <- purrr$map(
  .x = iso3_codes,
  .f = iso3_width_height
) |>
  purrr$list_rbind()

df_width_height$iso3 <- iso3_codes

# produce legend placement and alignment manually for countries

df_legend <- dplyr$tribble(
  ~iso3, ~legend_position, ~justification, ~direction,
  "AB9", "left", "top", "vertical",
  "ABW", "inside", c(1,1), "vertical",
  "AFG", "left", "top", "vertical",
  "AGO", "left", "top", "vertical",
  "AIA", "inside", c(0,1), "horizontal",
  "ALA", "top", "left", "horizontal",
  "AB9", "left", "top", "vertical",
  "ALB", "left", "top", "vertical",
  "AND", "left", "top", "vertical",
  "ARE", "inside", c(0,1), "horizontal",
  "ARG", "left", "top", "vertical",
  "ARM", "inside", c(0,1), "horizontal",
  "ASM", "inside", c(0.5, 0.5), "horizontal",
  "ATF", "inside", c(1,1), "horizontal",
  "ATG", "inside", c(0,1), "vertical",
  "AUS", "left", "top", "vertical",
  "AUT", "left", "top", "vertical",
  "AZE", "left", "top", "vertical",
  "BDI", "left", "top", "vertical",
  "BEL", "left", "top", "vertical",
  "BEN", "left", "top", "vertical",
  "BES", "inside", c(1,0), "vertical",
  "BFA", "left", "top", "vertical",
  "BGD", "left", "top", "vertical",
  "BGR", "left", "top", "vertical",
  "BHR", "left", "top", "vertical",
  "BHS", "inside", c(0,0), "horizontal",
  "BIH", "left", "top", "vertical",
  "BLM", "left", "top", "vertical",
  "BLR", "left", "top", "vertical",
  "BLZ", "left", "top", "vertical",
  "BMU", "inside", c(0,1), "horizontal",
  "BOL", "left", "top", "vertical",
  "BRA", "left", "top", "vertical",
  "BRB", "left", "top", "vertical",
  "BRN", "inside", c(0,1), "horizontal",
  "BTN", "left", "top", "vertical",
  "BVT", "left", "top", "vertical",
  "BWA", "left", "top", "vertical",
  "CAF", "left", "top", "vertical",
  "CAN", "left", "top", "vertical",
  "CCK", "inside", c(1,1), "horizontal",
  "CHE", "left", "top", "vertical",
  "CHL", "left", "top", "vertical",
  "CHN", "left", "top", "vertical",
  "CIV", "left", "top", "vertical",
  "CMR", "inside", c(0,1), "vertical",
  "COD", "left", "top", "vertical",
  "COG", "left", "top", "vertical",
  "COK", "inside", c(1,1), "horizontal",
  "COL", "inside", c(0,1), "vertical",
  "COM", "inside", c(1,1), "horizontal",
  "CPV", "inside", c(0,0), "vertical",
  "CRI", "inside", c(0,0), "horizontal",
  "CUB", "inside", c(0,0), "horizontal",
  "CUW", "inside", c(1,1), "horizontal",
  "CXR", "left", "top", "vertical",
  "CYM", "inside", c(1,0), "horizontal",
  "CYP", "left", "top", "vertical",
  "CZE", "left", "top", "vertical",
  "DEU", "left", "top", "vertical",
  "DJI", "left", "top", "vertical",
  "DMA", "left", "top", "vertical",
  "DNK", "left", "top", "vertical",
  "DOM", "left", "top", "vertical",
  "DZA", "left", "top", "vertical",
  "ECU", "left", "top", "vertical",
  "EGY", "left", "top", "vertical",
  "ERI", "inside", c(0,0), "horizontal",
  "ESH", "inside", c(1,0), "vertical",
  "ESP", "left", "top", "vertical",
  "EST", "left", "top", "vertical",
  "ETH", "left", "top", "vertical",
  "FIN", "left", "top", "vertical",
  "FJI", "left", "top", "vertical",
  "FLK", "left", "top", "vertical",
  "FRA", "left", "top", "vertical",
  "FRO", "left", "top", "vertical",
  "FSM", "top", "left", "horizontal",
  "GAB", "left", "top", "vertical",
  "GBR", "left", "top", "vertical",
  "GEO", "top", "right", "horizontal",
  "GGY", "inside", c(0,1), "horizontal",
  "GHA", "left", "top", "vertical",
  "GIB", "left", "top", "vertical",
  "GIN", "inside", c(0,0), "horizontal",
  "GLP", "left", "top", "vertical",
  "GMB", "top", "left", "horizontal",
  "GNB", "left", "top", "vertical",
  "GNQ", "inside", c(0,0), "vertical",
  "GRC", "top", "left", "horizontal",
  "GRD", "inside", c(0,1), "vertical",
  "GRL", "left", "top", "vertical",
  "GTM", "left", "top", "vertical",
  "GUF", "left", "top", "vertical",
  "GUM", "left", "top", "vertical",
  "GUY", "left", "top", "vertical",
  "HKG", "left", "top", "vertical",
  "HMD", "left", "top", "vertical",
  "HND", "left", "top", "vertical",
  "HRV", "left", "top", "vertical",
  "HTI", "inside", c(0,1), "vertical",
  "HUN", "top", "left", "horizontal",
  "IDN", "top", "left", "horizontal",
  "IMN", "left", "top", "vertical",
  "IND", "left", "top", "vertical",
  "IOT", "left", "top", "vertical",
  "IRL", "left", "top", "vertical",
  "IRN", "left", "top", "vertical",
  "IRQ", "left", "top", "vertical",
  "ISL", "left", "top", "vertical",
  "ISR", "left", "top", "vertical",
  "ITA", "left", "top", "vertical",
  "JAM", "top", "left", "horizontal",
  "JEY", "left", "top", "vertical",
  "JOR", "left", "top", "vertical",
  "JPN", "inside", c(0,1), "vertical",
  "KAZ", "left", "top", "vertical",
  "KEN", "left", "top", "vertical",
  "KGZ", "top", "left", "horizontal",
  "KHM", "left", "top", "vertical",
  "KIR", "top", "left", "horizontal",
  "KNA", "inside", c(0,0), "vertical",
  "KOR", "left", "top", "vertical",
  "KWT", "left", "top", "vertical",
  "LAC", "left", "top", "vertical",
  "LAO", "inside", c(0,0), "horizontal",
  "LBN", "left", "top", "vertical",
  "LBR", "inside", c(0,0), "horizontal",
  "LBY", "inside", c(0,0), "horizontal",
  "LCA", "left", "top", "vertical",
  "LIE", "left", "top", "vertical",
  "LKA", "left", "top", "vertical",
  "LSO", "left", "top", "vertical",
  "LTU", "left", "top", "vertical",
  "LUX", "left", "top", "vertical",
  "LVA", "top", "left", "horizontal",
  "MAC", "left", "top", "vertical",
  "MAF", "top", "left", "horizontal",
  "MAR", "inside", c(0,1), "vertical",
  "MCO", "left", "top", "vertical",
  "MDA", "inside", c(0,0), "vertical",
  "MDG", "left", "top", "vertical",
  "MDV", "left", "top", "vertical",
  "MEX", "left", "top", "vertical",
  "MHL", "inside", c(0,0), "vertical",
  "MKD", "left", "top", "vertical",
  "MLI", "left", "top", "vertical",
  "MLT", "inside", c(0,0), "vertical",
  "MMR", "left", "top", "vertical",
  "MNE", "left", "top", "vertical",
  "MNG", "top", "left", "horizontal",
  "MNP", "left", "top", "vertical",
  "MOZ", "inside", c(1,0), "vertical",
  "MRT", "left", "top", "vertical",
  "MSR", "left", "top", "vertical",
  "MTQ", "inside", c(0,0), "vertical",
  "MUS", "inside", c(1,1), "horizontal",
  "MWI", "left", "top", "vertical",
  "MYS", "inside", c(0.5,1), "horizontal",
  "MYT", "left", "top", "vertical",
  "NAM", "left", "top", "vertical",
  "NCL", "inside", c(0,0), "horizontal",
  "NER", "inside", c(0,1), "horizontal",
  "NFK", "left", "top", "vertical",
  "NGA", "left", "top", "vertical",
  "NIC", "inside", c(0,1), "horizontal",
  "NIU", "left", "top", "vertical",
  "NLD", "left", "top", "vertical",
  "NOR", "inside", c(1,0), "vertical",
  "NPL", "top", "left", "horizontal",
  "NRU", "left", "top", "vertical",
  "NZL", "inside", c(0,1), "vertical",
  "OMN", "inside", c(0,1), "vertical",
  "PAK", "inside", c(0,1), "vertical",
  "PAN", "top", "left", "horizontal",
  "PCN", "left", "top", "vertical",
  "PER", "left", "top", "vertical",
  "PHL", "left", "top", "vertical",
  "PLW", "inside", c(0,1), "vertical",
  "PNG", "top", "left", "horizontal",
  "POL", "left", "top", "vertical",
  "PRI", "top", "left", "horizontal",
  "PRK", "inside", c(0,1), "horizontal",
  "PRT", "left", "top", "vertical",
  "PRY", "left", "top", "vertical",
  "PSE", "inside", c(0,1), "vertical",
  "PYF", "inside", c(0,1), "horizontal",
  "QAT", "left", "top", "vertical",
  "REU", "left", "top", "vertical",
  "ROU", "left", "top", "vertical",
  "RUS", "top", "left", "horizontal",
  "RWA", "inside", c(0,1), "horizontal",
  "SAU", "inside", c(1,1), "horizontal",
  "SDN", "left", "top", "vertical",
  "SEN", "left", "top", "vertical",
  "SGP", "top", "left", "horizontal",
  "SGS", "inside", c(0,0), "horizontal",
  "SHN", "inside", c(0,0.5), "vertical",
  "SJM", "inside", c(0,1), "vertical",
  "SLB", "inside", c(1,1), "horizontal",
  "SLE", "left", "top", "vertical",
  "SLV", "top", "left", "horizontal",
  "SMR", "left", "top", "vertical",
  "SOM", "inside", c(1,0), "vertical",
  "SPM", "left", "top", "vertical",
  "SRB", "left", "top", "vertical",
  "SSD", "left", "top", "vertical",
  "STP", "inside", c(0,1), "vertical",
  "SUR", "left", "top", "vertical",
  "SVK", "top", "left", "horizontal",
  "SVN", "top", "left", "horizontal",
  "SWE", "left", "top", "vertical",
  "SWZ", "left", "top", "vertical",
  "SXM", "top", "left", "horizontal",
  "SYC", "inside", c(0,1), "horizontal",
  "SYR", "left", "top", "vertical",
  "TCA", "top", "left", "horizontal",
  "TCD", "left", "top", "vertical",
  "TGO", "left", "top", "vertical",
  "THA", "left", "top", "vertical",
  "TJK", "top", "left", "horizontal",
  "TKL", "inside", c(1,1), "horizontal",
  "TKM", "top", "left", "horizontal",
  "TLS", "top", "left", "horizontal",
  "TON", "left", "top", "vertical",
  "TTO", "inside", c(0,1), "horizontal",
  "TUN", "left", "top", "vertical",
  "TUR", "top", "left", "horizontal",
  "TUV", "inside", c(1,1), "vertical",
  "TWN", "left", "top", "vertical",
  "TZA", "left", "top", "vertical",
  "UGA", "left", "top", "vertical",
  "UKR", "top", "left", "horizontal",
  "UMI", "top", "left", "horizontal",
  "URY", "left", "top", "vertical",
  "USA", "top", "left", "horizontal",
  "UZB", "top", "left", "horizontal",
  "UKR", "top", "left", "horizontal",
  "VAT", "top", "left", "horizontal",
  "VCT", "inside", c(0,1), "vertical",
  "VEN", "left", "top", "vertical",
  "VGB", "inside", c(0,0.85), "horizontal",
  "VIR", "inside", c(0.5,0.5), "horizontal",
  "VNM", "inside", c(0,0), "vertical",
  "VUT", "inside", c(1,1), "vertical",
  "WLF", "inside", c(0,1), "horizontal",
  "WSM", "top", "left", "horizontal",
  "XKX", "left", "top", "vertical",
  "YEM", "top", "left", "horizontal",
  "ZAF", "top", "left", "horizontal",
  "ZMB", "top", "left", "horizontal",
  "ZWE", "left", "top", "vertical",
)

# add legend location based on its placement
df_legend$location <- ifelse(
  df_legend$legend_position == "inside",
  "panel",
  "plot"
)

# change justification to be single numeric vector based on left/top
# so that legen isn't so far in the corners
df_legend$justification_numeric <- purrr$map(
  .x = df_legend$justification,
  .f = \(x) {
    if (length(x) > 1) {
      x
    } else if (x == "top") {
      0.9
    } else if (x == "left") {
      0.1
    }
  }
)

df_map_settings <- dplyr$left_join(
  df_legend,
  df_width_height,
  by = "iso3"
)

cs$update_az_file(
  df = df_map_settings,
  name = "input/iso3_map_settings.json"
)