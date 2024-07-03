box::use(../../src/indicators/ipc_food_insecurity/utils/raw_food_insecurity)

df_raw <- raw_agricultural_hotspots$raw()
df_raw_subset <- head(df_raw, 100)
saveRDS(df_raw_subset, "test/data/jrc_hotspots_basic.RDS")
