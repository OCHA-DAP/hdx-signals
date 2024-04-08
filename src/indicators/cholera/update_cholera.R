box::use(cs = ../../utils/cloud_storage)
box::use(./utils/raw_cholera)
box::use(./utils/wrangle_cholera)
box::use(./utils/alert_cholera)

df_wrangled <- raw_cholera$raw() |>
  wrangle_cholera$wrangle()

df_alerts <- alert_cholera$alert(
  df_wrangled = df_wrangled,
    recreate = TRUE
  )

############################
#### SAVE CHOLERA  DATA ####
############################

cs$update_az_file(
  df = df_wrangled,
  name = "output/cholera/wrangled.parquet"
)

cs$update_az_file(
  df = df_alerts,
  name = "output/cholera/flags.parquet"
)
