library(idmc)
library(tidyverse)

output_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "exploration",
  "glb",
  "global_monitoring",
  "outputs"
)

##############
#### IDMC ####
##############

df_idmc_raw <- idmc_get_data()

df_idmc <- df_idmc_raw %>%
  idmc_transform_daily() %>%
  idmc_rolling_sum() %>%
  idmc_flagging()

# flagging

df_idmc_flags <- df_idmc %>%
  select(
    -flag_total
  ) %>%
  rename_with(
    .fn = ~gsub("(flag_)(.*)", "\\1idmc_\\2", .x),
    .cols = starts_with("flag")
  ) %>%
  pivot_longer(
    cols = starts_with("flag"),
    names_to = "flag_type",
    values_to = "flag"
  ) %>%
  select(
    -starts_with('displacement')
  ) %>%
  group_by(
    iso3,
    country,
    flag_type
  ) %>%
  filter(
    flag
  ) %>%
  mutate(
    cs_temp_ = cumsum(date - lag(date, default = min(date)) != lubridate::days(1))
  ) %>%
  group_by(
    iso3,
    country,
    flag_type,
    cs_temp_
  ) %>%
  summarize(
    start_date = min(date),
    end_date = max(date),
    .groups = "drop"
  ) %>%
  select(
    -cs_temp_
  ) %>%
  mutate(
    flag_source = "idmc",
    .before = flag_type
  )

#########################
#### SAVING OUT DATA ####
#########################

write_csv(
  x = df_idmc_raw,
  file = file.path(
    output_dir,
    paste0(Sys.Date(), "_idmc_raw.csv")
  )
)

write_csv(
  x = df_idmc,
  file = file.path(
    output_dir,
    paste0(Sys.Date(), "_idmc_rolling.csv")
  )
)

write_csv(
  x = df_idmc_flags,
  file = file.path(
    output_dir,
    paste0(Sys.Date(), "_idmc_flags.csv")
  )
)
