box::use(
  src/utils/cloud_storage,
  src/utils/read_manual_info,
  src/utils/get_manual_info,
  src/utils/validate_manual_info
)


new_row <- data.frame(
  iso3 = "AFG",
  indicator_id = "idmc_displacement_conflict",
  date = as.Date("2025-11-05"),
  text1 = "test",
  text2 = "test",
  stringsAsFactors = FALSE
)

# 1. Read existing file
df_existing <- read_manual_info$read_manual_info()

# 2. Add new row
df_updated <- rbind(df_existing, new_row)

# 3. Update file
cloud_storage$update_az_file(
  df = df_updated,
  name = "input/manual_context_info.csv",
  container = "dev"
)
