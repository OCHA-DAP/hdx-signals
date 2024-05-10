box::use(dplyr)

#' Wrangle drought data
#'
#' Does very little, as data is essentially ready for alerting and plotting. For
#' now adds an ISO3 code.
#'
#' @param df_raw Raw drought data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_raw |>
    dplyr$transmute(
      iso3 = dplyr$case_when(
        !is.na(ISO3) ~ ISO3,
        asap0_name == "Laos" ~ "LAO",
        asap0_name == "Central Africa" ~ "CAF",
        asap0_name == "D.R. Congo" ~ "COD",
        asap0_name == "North Korea" ~ "PRK",
        asap0_name == "Equat. Guinea" ~ "GNQ"
      ),
      date,
      comment,
      hs_code,
      hs_name
    ) |>
    dplyr$arrange(
      iso3,
      date
    )
}
