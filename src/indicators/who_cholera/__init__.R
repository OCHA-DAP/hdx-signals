#' @export
box::use(
  src/indicators/who_cholera/utils/alert_cholera[...],
  src/indicators/who_cholera/utils/wrangle_cholera[...],
  src/indicators/who_cholera/utils/info_cholera[...],
  src/indicators/who_cholera/utils/plot_cholera[...],
  src/indicators/who_cholera/utils/raw_cholera[...],
  src/indicators/who_cholera/utils/summary_cholera[...],
)

#' @export
indicator_id <- "who_cholera"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/who_cholera,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("NGA", "SSD")
  )
}
