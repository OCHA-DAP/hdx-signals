box::use(./utils/raw_food_insecurity)
box::use(./utils/wrangle_food_insecurity)
box::use(./utils/alert_food_insecurity)

box::use(../../alerts/generate_signals[generate_signals])

first_run <- TRUE

df_raw <- raw_food_insecurity$raw()
df_wrangled <- wrangle_food_insecurity$wrangle(df_raw)

# now generate signals
generate_signals(
  df_wrangled = df_wrangled,
  indicator_id = "ipc_food_insecurity",
  alert_fn = alert_food_insecurity$alert,
  first_run = first_run
)
