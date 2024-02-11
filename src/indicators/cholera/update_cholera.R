box::use(cs = ../../utils/cloud_storage)
box::use(./raw_cholera)
box::use(./wrangle_cholera)
box::use(./alert_cholera)

df_wrangled <- raw_cholera$raw() |>
  wrangle_cholera$wrangle()

df_alerts <- alert_cholera$alert(
  df_wrangled = df_wrangled,
    recreate = TRUE
  )

############################
#### SAVE CHOLERA  DATA ####
############################

cs$update_gcs_file(
  df = df_wrangled,
  name = "output/cholera/wrangled.parquet"
)

cs$update_gcs_file(
  df = df_alerts,
  name = "output/cholera/flags.parquet"
)
