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
  # add in link information for later usage
  # get the links based on the analysis ID
  df_links <- ripc$ipc_get_analyses() |>
    dplyr$select(
      analysis_id,
      link
    )

  df_raw |>
    dplyr$group_by(
      country, analysis_date, period
    ) |>
    dplyr$filter( # drop a few cases of duplicate analyses on the same date, keep larger coverage
      estimated_population == max(estimated_population)
    ) |>
    dplyr$ungroup() |>
    dplyr$left_join(
      df_links,
      by = "analysis_id"
    ) |>
    dplyr$mutate(
      from = ifelse(
        readr$parse_number(from) == readr$parse_number(to), # identifying same year
        stringr$str_remove(from, " [\\d]+"),
        from
      ),
      map_date = paste(from, to, sep = " to "),
      coverage = estimated_population / ifelse( # % of pop analyzed
        population < estimated_population,
        estimated_population,
        population
      ),
      title_suffix = stringr$str_extract(
        title,
        "(?<=[0-9]{4} )(.*)"
      ) |> stringr$str_remove_all(
        "\\(|\\)"
      ) |>
        stringr$str_to_title(),
      analysis_area = dplyr$case_when( # subarea specified in some IPC publications
        title_suffix == "" ~ NA_character_, # empty string
        stringr$str_detect(title_suffix, "Proj|proj") ~ NA_character_, # projection updates
        stringr$str_detect(title_suffix, "[0-9]{4}") ~ NA_character_, # just versioning of BDI
        stringr$str_detect(title_suffix, "ème") ~ NA_character_, # versioning for COD
        stringr$str_detect(title_suffix, "è Cycle") ~ NA_character_, # versioning for COD
        title_suffix %in% c("LRA", "SRA") ~ NA_character_, # versioning for KEN
        !is.na(title_suffix) ~ title_suffix,
        .default = NA_character_
      )
    ) |>
    dplyr$select(
      iso3,
      date,
      analysis_id,
      plot_date = analysis_period_start,
      map_date,
      period,
      coverage,
      analysis_area,
      link,
      dplyr$matches("(phase[3-5]|plus)_percentage")
    ) |>
    dplyr$group_by(
      iso3
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
      values_from = c(percentage, plot_date, map_date, analysis_area, coverage),
      names_sep = "-"
    ) |>
    dplyr$group_by(
      iso3, phase
    ) |>
    dplyr$mutate( # use lages to compare between different analyses
      `percentage-current_lag` = dplyr$lag(`percentage-current`),
      `coverage-current_lag` = dplyr$lag(`coverage-current`),
      `analysis_area-current_lag` = dplyr$lag(`analysis_area-current`),
      compare_current = is.na(`analysis_area-current`) == is.na(`analysis_area-current_lag`) &
        abs(`coverage-current` - `coverage-current_lag`) <= 0.1 # if we can draw comparisons
    ) |>
    dplyr$ungroup()
}
