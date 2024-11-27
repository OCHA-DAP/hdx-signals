#' @export
box::use(
  src/indicators/ipc_food_insecurity/utils/alert_food_insecurity[...],
  src/indicators/ipc_food_insecurity/utils/wrangle_food_insecurity[...],
  src/indicators/ipc_food_insecurity/utils/info_food_insecurity[...],
  src/indicators/ipc_food_insecurity/utils/map_food_insecurity[...],
  src/indicators/ipc_food_insecurity/utils/plot_food_insecurity[...],
  src/indicators/ipc_food_insecurity/utils/raw_food_insecurity[...],
  src/indicators/ipc_food_insecurity/utils/summary_food_insecurity[...],
  src/indicators/ipc_food_insecurity/utils/wrangle_food_insecurity[...]
)

#' @export
indicator_id <- "ipc_food_insecurity"

if (is.null(box::name())) {
  box::use(
    module = src/indicators/ipc_food_insecurity,
    src/signals
  )

  signals$generate_signals(
    ind_module = module,
    dry_run_filter = c("AFG", "SSD")
  )
}
