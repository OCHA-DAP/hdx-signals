box::use(cs = src/utils/cloud_storage)

#' Download raw SEA5 anomaly data
#'
#' Reads the ADM1-level SEAS5 forecast skill statistics written by the
#' `ds-seas5-skill` pipeline (`pipeline/compute_skill_adm1.py`) to the shared
#' `projects` container on the dev storage account. One row per
#' (`pcode`, `issued_month`, `trimester`), carrying the skill metrics and the
#' position of the latest forecast within that combination's own history.
#'
#' @export
raw <- function() {
  cs$read_az_file(
    "ds-seas5-skill/processed/skill_stats_adm1.parquet",
    container = "projects"
  )
}
