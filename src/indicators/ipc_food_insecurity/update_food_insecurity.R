box::use(./utils/raw_food_insecurity)
box::use(./utils/wrangle_food_insecurity)
box::use(./utils/alert_food_insecurity)
box::use(./utils/plot_food_insecurity)
box::use(./utils/summary_food_insecurity)
box::use(./utils/map_food_insecurity)

box::use(../../alerts/generate_signals[generate_signals])

df_raw <- raw_food_insecurity$raw()
df_wrangled <- wrangle_food_insecurity$wrangle(df_raw)

# now generate signals
df_ipc <- generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "ipc_food_insecurity",
  alert_fn = alert_food_insecurity$alert,
  plot_fn = plot_food_insecurity$plot,
  summary_fn = summary_food_insecurity$summary,
  map_fn = map_food_insecurity$map,
  test = TRUE
)
