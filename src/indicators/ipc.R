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


ipc_messager <- function(type, pct_increase, phase, num, pct_pop) {
  paste(
    "In the IPC",
    type,
    "analysis, there is an estimated",
    pct_increase,
    "in population in phase",
    phase,
    ", to a total of",
    num,
    "or",
    pct_pop,
    "of the population."
  )
}

#############
#### IPC ####
#############

ipc_messager <- function(type, pct_increase, phase, num, pct_pop) {
  paste(
    "In the IPC",
    type,
    "analysis, there is an estimated",
    pct_increase,
    "in population in phase",
    phase,
    ", to a total of",
    num,
    "or",
    pct_pop,
    "of the population."
    )
}

# get into country level form
df_ipc_raw <- ipc_download()

df_ipc <- df_ipc_raw %>%
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

# get differences between current values and the previous current value
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
      ~ .x - lag(.x),
      .names = "{col}_delta"
    )
  ) %>%
  select(
    country:analysis_type,
    ends_with("delta")
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
      ~ .x - lag(.x),
      .names = "{col}_delta"
    )
  ) %>%
  select(
    country:analysis_type,
    ends_with("delta")
  ) %>%
  slice(-1) %>%
  ungroup()

######################
#### WRANGLE DATA ####
######################

# wrangle the data together for a single country-level IPC dataset

df_ipc_wrangled <- left_join(
  df_ipc,
  bind_rows(df_cur_delta, df_proj_delta),
  by = c(
    "country",
    "iso3",
    "date_of_analysis",
    "analysis_period_start",
    "analysis_period_end",
    "analysis_type"
  )
)

#######################
#### FLAGGING DATA ####
#######################

ipc_messager <- function(type, pct_increase, phase, num, pct_pop, start_date, end_date) {
  phase <- str_replace_all(phase, c("_" = " ", "pl" = "+"))
  paste(
    "There is an expected increase of",
    scales::percent(pct_increase, accuracy = 1),
    "of population in",
    phase,
    "during the",
    str_replace(type, "_", " "),
    "period from",
    format(start_date, format = "%B %Y"),
    "to",
    format(end_date, format = "%B %Y"),
    "compared to the",
    case_when(
      type == "current" ~ "previous current",
      type == "first_projection" ~ "current",
      TRUE ~ "first projection"
    ),
    "period. The total population in",
    phase,
    "is estimated to be",
    scales::number(num, big.mark = ","),
    "people,",
    scales::percent(pct_pop, accuracy = 1),
    "of the population."
  )
}

# flag on the IPC data by looking at wherever there is a positive percent change
df_ipc_flags <- df_ipc_wrangled %>%
  pivot_longer(
    cols = starts_with("phase"),
    names_to = c("phase", "name"),
    names_pattern = "(phase_[0-9]+[pl]*)_(.*)"
  ) %>%
  pivot_wider() %>%
  filter(
    !is.na(pct_delta),
    pct_delta > 0,
    str_detect(phase, "3pl|4pl|5")
  ) %>%
  mutate(
    message = ipc_messager(
      type = analysis_type,
      pct_increase = pct_delta,
      phase = phase,
      num = num,
      pct_pop = pct,
      start_date = analysis_period_start,
      end_date = analysis_period_end
    )
  ) %>%
  group_by(
    country,
    iso3,
    start_date = analysis_period_start
  ) %>%
  summarize(
    end_date = max(analysis_period_end),
    message = paste(message, collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(
    flag_type = "displacement",
    flag_source = "ipc",
    .before = "start_date"
  )


########################
#### SAVE IPC  DATA ####
########################

write_csv(
  x = df_ipc_raw,
  file = file.path(
    output_dir,
    "ipc",
    "raw.csv"
  )
)

write_csv(
  x = df_ipc_wrangled,
  file = file.path(
    output_dir,
    "ipc",
    "wrangled.csv"
  )
)

write_csv(
  x = df_ipc_flags,
  file = file.path(
    output_dir,
    "ipc",
    "flags.csv"
  )
)
