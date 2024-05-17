box::use(dplyr)
box::use(tidyr)
box::use(stringr)
box::use(readr)
box::use(ripc)

#' Wrangle food insecurity data
#'
#' FILL IN
#'
#' @param df_raw Raw food insecurity data frame
#'
#' @returns Wrangled data frame
#'
#' @export
wrangle <- function(df_raw) {
  df_wrangled <- df_raw |>
    dplyr$group_by(
      iso3
    ) |>
    dplyr$mutate(
      from = ifelse(
        readr$parse_number(from) == readr$parse_number(to), # identifying same year
        stringr$str_remove(from, " [\\d]+"),
        from
      ),
      map_date = paste(from, to, sep = " to ")
    ) |>
    dplyr$select(
      iso3,
      date,
      analysis_id,
      plot_date = analysis_period_start,
      map_date,
      period,
      dplyr$matches("(phase[3-5]|plus)_percentage")
    ) |>
    dplyr$arrange(
      date,
      .by_group = TRUE
    ) |>
    tidyr$pivot_longer(
      cols = ends_with("percentage"),
      names_to = "phase",
      names_transform = \(x) stringr$str_remove(x, "_percentage"),
      values_to = "percentage"
    ) |>
    tidyr$pivot_wider(
      names_from = period,
      values_from = c(percentage, plot_date, map_date),
      names_sep = "-"
    ) |>
    dplyr$group_by(
      iso3, phase
    ) |>
    dplyr$mutate(
      `percentage-current_lag` = dplyr$lag(`percentage-current`)
    ) |>
    dplyr$ungroup()

  # add in link information for later usage
  # get the links based on the analysis ID
  df_links <- ripc$ipc_get_analyses() |>
    dplyr$select(
      analysis_id,
      title,
      link
    )

  # this is used so that we can differentiate areas analyzed differently
  df_wrangled <- df_wrangled |>
    dplyr$left_join(
      df_links,
      by = "analysis_id"
    ) |>
    dplyr$mutate(
      title_suffix = stringr$str_extract(
        title,
        "(?<=[0-9]{4} )(.*)"
      ) |> stringr$str_remove_all(
        "\\(|\\)"
      ),
      analysis_area = dplyr$case_when( # subarea specified in some IPC publications
        stringr$str_detect(title_suffix, "[0-9]{4}") ~ NA_character_, # just versioning of BDI
        stringr$str_detect(title_suffix, "ème") ~ NA_character_, # versioning for COD
        title_suffix %in% c("LRA", "SRA") ~ NA_character_, # versioning for KEN
        stringr$str_detect(tolower(title_suffix), "partial") ~ "", # flag this, but no text to show
        !is.na(title_suffix) ~ title_suffix,
        .default = NA_character_
      )
    ) |>
    dplyr$filter(
      !(is.na(title) & iso3 == "TCD") # removing temporary Chad analysis that isn't in sync with timeline
    )

  df_wrangled
}
