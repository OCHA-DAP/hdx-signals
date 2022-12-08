library(Ripc)
library(tidyverse)
library(countrycode)

output_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "exploration",
  "glb",
  "global_monitoring",
  "outputs"
)

#############
#### IPC ####
#############

# get into country level form
df_ipc <- ipc_download() %>%
  mutate(
    iso3 = countrycode(country, origin = "country.name", destination = "iso3c"),
    phase_4pl_num = phase_4_num + phase_5_num,
    phase_4pl_pct = phase_4_pct + phase_5_pct
  ) %>%
  group_by(
    country,
    iso3,
    date_of_analysis,
    analysis_period_start,
    analysis_period_end,
    analysis_type
  ) %>%
  summarize(
    across(
      .cols = ends_with("_pct"),
      .fns = weighted.mean,
      w = population,
      na.rm = TRUE
    ),
    across(
      .cols = c(ends_with("_num"), population),
      .fns = sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# get differences between current values
df_cur_delta <- df_ipc %>%
  filter(
    analysis_type == "current"
  ) %>%
  group_by(
    country,
    iso3
  ) %>%
  arrange(
    date_of_analysis,
    .by_group = TRUE
  ) %>%
  mutate(
    across(
      matches("pct$"),
      ~ .x - lag(.x)
    ),
    flag_ipc_curr_3pl = phase_3pl_pct > 0,
    flag_ipc_curr_4pl = phase_4pl_pct > 0,
    flag_ipc_curr_5 = phase_5_pct > 0,
  ) %>%
  select(
    country:analysis_period_end,
    ends_with("pct"),
    starts_with("flag")
  ) %>%
  slice(-1) %>%
  ungroup()

# get differences between current and projections (or first to second proj)
df_proj_delta <- df_ipc %>%
  group_by(
    country,
    iso3,
    date_of_analysis
  ) %>%
  filter(
    "first_projection" %in% analysis_type
  ) %>%
  arrange(
    analysis_type,
    .by_group = TRUE
  ) %>%
  mutate(
    across(
      ends_with("pct"),
      ~ .x - lag(.x)
    ),
    flag_ipc_proj_3pl = phase_3pl_pct > 0,
    flag_ipc_proj_4pl = phase_4pl_pct > 0,
    flag_ipc_proj_5 = phase_5_pct > 0,
  ) %>%
  select(
    country:analysis_type,
    ends_with("pct"),
    starts_with("flag"),
  ) %>%
  slice(-1) %>%
  ungroup()

#######################
#### FLAGGING DATA ####
#######################

# pull all IPC flags together
# and put the start date of the
# flags as the date of analysis
# and the end date to be 1 month
# later

df_cur_flags <- df_cur_delta %>%
  select(
    country,
    iso3,
    date_of_analysis,
    starts_with("flag")
  )

df_proj1_flags <- df_proj_delta %>%
  filter(
    analysis_type == "first_projection"
  ) %>%
  select(
    country,
    iso3,
    date_of_analysis,
    starts_with("flag")
  ) %>%
  rename_with(
    .fn = ~gsub("(flag_ipc_proj)(.*)", "\\11\\2", .x),
    .cols = starts_with("flag")
  )

df_proj2_flags <- df_proj_delta %>%
  filter(
    analysis_type == "second_projection"
  ) %>%
  select(
    country,
    iso3,
    date_of_analysis,
    starts_with("flag")
  ) %>%
  rename_with(
    .fn = ~gsub("(flag_ipc_proj)(.*)", "\\12\\2", .x),
    .cols = starts_with("flag")
  )

reduce(
  list(df_cur_flags, df_proj1_flags, df_proj2_flags),
  full_join,
  by = c("country", "iso3", "date_of_analysis")
)

df_ipc_flags <- reduce(
  list(df_cur_flags, df_proj1_flags, df_proj2_flags),
  full_join,
  by = c("country", "iso3", "date_of_analysis")
) %>%
  select(
    country,
    iso3,
    date_of_analysis,
    starts_with("flag")
  ) %>%
  rowwise() %>%
  mutate(
    flag_ipc_any = sum(c_across(starts_with("flag")), na.rm = TRUE) > 0
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("flag"),
    names_to = "flag_type",
    values_to = "flag"
  ) %>%
  rename(
    start_date = date_of_analysis
  ) %>%
  mutate(
    end_date = as.Date(start_date + lubridate::dmonths()),
    .after = start_date
  ) %>%
  filter(
    flag
  ) %>%
  mutate(
    flag_source = "ipc",
    .before = flag_type
  ) %>%
  select(
    -flag
  )

########################
#### SAVE IPC  DATA ####
########################

today <- Sys.Date()

write_csv(
  x = df_ipc,
  file = file.path(
    output_dir,
    paste0(today, "_ipc_global.csv")
  )
)

write_csv(
  x = df_cur_delta,
  file = file.path(
    output_dir,
    paste0(today, "_ipc_curr_delta.csv")
  )
)

write_csv(
  x = df_proj_delta,
  file = file.path(
    output_dir,
    paste0(today, "_ipc_proj_delta.csv")
  )
)

write_csv(
  x = df_ipc_flags,
  file = file.path(
    output_dir,
    paste0(today, "_ipc_flags.csv")
  )
)
