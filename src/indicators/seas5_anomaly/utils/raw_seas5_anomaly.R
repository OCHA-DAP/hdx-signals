box::use(cs = src/utils/cloud_storage)

#' Download raw SEAS5 anomaly data
#'
#' Reads the latest ADM1-level HDX Signals inputs written by the
#' `ds-seas5-skill` pipeline to the shared `projects` container on the dev
#' storage account. The file holds only the most recent SEAS5 issuance, with
#' one row per (`pcode`, `trimester`) for every trimester the issuance covers
#' (`lead` is negative for trimesters already underway). Alongside the
#' identifiers (`iso3`, `name`, `issued_year`, `issued_month`, `season_year`)
#' each row carries the forecast skill (`pearson_r`, `n_years`, `in_sample`),
#' the forecast anomaly expressed as a historical percentile (`pct`) and as
#' dry and wet return periods (`dry_rp`, `wet_rp`), and the rainy-season flags
#' `in_season_flat` and `in_season_app` with the climatology behind them
#' (`tri_share_annual`, `tri_mean_mm_day`).
#'
#' @export
raw <- function() {
  cs$read_az_file(
    "ds-seas5-skill/processed/hdx_signal/signal_inputs_adm1_latest.parquet",
    container = "projects"
  )
}
