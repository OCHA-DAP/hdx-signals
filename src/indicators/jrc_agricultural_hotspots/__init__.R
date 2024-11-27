#' @export
box::use(
  src/indicators/jrc_agricultural_hotspots/utils/alert_agricultural_hotspots[...],
  src/indicators/jrc_agricultural_hotspots/utils/wrangle_agricultural_hotspots[...],
  src/indicators/jrc_agricultural_hotspots/utils/info_agricultural_hotspots[...],
  src/indicators/jrc_agricultural_hotspots/utils/plot_agricultural_hotspots[...],
  src/indicators/jrc_agricultural_hotspots/utils/raw_agricultural_hotspots[...],
  src/indicators/jrc_agricultural_hotspots/utils/summary_agricultural_hotspots[...],
  src/indicators/jrc_agricultural_hotspots/utils/wrangle_agricultural_hotspots[...]
)

#' @export
indicator_id <- "jrc_agricultural_hotspots"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/jrc_agricultural_hotspots,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("AFG", "SSD")
  )
}
