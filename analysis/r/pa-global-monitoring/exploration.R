library(jsonlite)
library(httr)
library(tidyverse)
library(zoo)
library(lubridate)
library(gghdx)
library(forecast)
library(here)

gghdx()

# utility functions
source("utils.R")

# global monitoring folder for outputting plots
gm_dir <- Sys.getenv("CERF_GM_DIR")
out_dir <- here(
  gm_dir,
  "output"
)
plot_dir <- here(
  out_dir,
  "plots"
)

data_dir <- here(
  out_dir,
  "data"
)

###################
#### LOAD DATA ####
###################

IDMC_URL <- "https://backend.idmcdb.org/data/idus_view_all_flat_cached_ochachd"

df <- parse_json(
  GET(IDMC_URL),
  simplifyVector = TRUE
) %>%
  tibble()


############################
#### CREATE NEW DATASET ####
############################

# create new daily data of displacement, allocating displacement uniformly
# across the days from start to end date for each reported event
# and then summing across days

df_daily <- df %>%
  mutate(
    across(
      contains("date"),
      as.Date
    ),
    start_date = if_else(
      displacement_start_date <= displacement_end_date,
      displacement_start_date,
      displacement_end_date
    ),
    end_date = if_else(
      displacement_start_date <= displacement_end_date,
      displacement_end_date,
      displacement_start_date
    )
  ) %>%
  filter( # remove few cases where both start and end not available
    !is.na(start_date),
    !is.na(end_date)
  ) %>%
  rowwise() %>%
  transmute(
    country,
    id,
    date = list(
      seq(start_date, end_date, by = "day") # create daily cases
    ),
    figure = figure / length(date) # uniform distribute figures across days
  ) %>%
  ungroup() %>%
  unnest(
    cols = date
  ) %>%
  group_by(
    country,
    date
  ) %>%
  summarize(
    figure = sum(figure),
    .groups = "drop"
  )

# create a complete dataset from the first reported date until the last
# reported date (instead of today) with 0s

df_daily_complete <- df_daily %>%
  complete(
    country,
    date = seq(min(date), max(date), by = "day"),
    fill = list(
      figure = 0
    )
  ) %>%
  arrange( # ensuring data is correctly ordered for later rolling sums
    country,
    date
  )

#########################
#### AGGREGATED DATA ####
#########################

# now that we have all cases by day, can smooth to different temporal scales
# as a rolling sum

df_rolled <- df_daily_complete %>%
  group_by(
    country
  ) %>%
  mutate(
    weekly = rollsumr(
      x = figure,
      k = 7,
      na.pad = TRUE
    ),
    monthly = rollsumr(
      x = figure,
      k = 30,
      na.pad = TRUE
    ),
    quarterly = rollsumr(
      x = figure,
      k = 90,
      na.pad = TRUE
    ),
    yearly = rollsumr(
      x = figure,
      k = 365,
      na.pad = TRUE
    )
  ) %>%
  rename(
    daily = figure
  )

###################################
#### CREATE DATASET WITH FLAGS ####
###################################

df_flagged <- df_rolled %>%
  mutate(
    first_displacement_in_year = daily > 0 & lag(yearly) == 0,
    flag_daily = flag_percent(daily),
    flag_monthly = flag_percent(monthly),
    flag_quarterly = flag_percent(quarterly),
    flag_yearly = flag_percent(yearly),
    ts_outliers = tsoutliers_clean(x = daily, row_numbers = row_number())
  ) %>%
  ungroup() %>%
  mutate(
    flag_daily_global = flag_percent(daily),
    flag_monthly_global = flag_percent(monthly),
    flag_quarterly_global = flag_percent(quarterly),
    flag_yearly_global = flag_percent(yearly),
    ts_outliers_global = tsoutliers_clean(x = daily, row_numbers = row_number()),
    flag_5k_day = daily >= 5000,
    flag_20k_week = weekly >= 20000,
    flag_100k_quarter = quarterly >= 100000,
    flag_500k_yearly = yearly >= 500000
  )

write_csv(
  df_flagged,
  here(
    data_dir,
    "df_flagged_normalized.csv"
  )
)

# create data for limited countries and years just for testing of methods
# and easy visualization. Only compute total flags on this filtered dataset
# to improve performance

# data is flagged on first displacement in year,
# country-level displacement figures in 95th percentile
# for daily, weekly, quarterly, and yearly, and
# absolute thresholds for each
df_filtered <- df_flagged %>%
  filter(
    country %in% c(
      "Sri Lanka",
      "Iraq",
      "Syria",
      "Myanmar",
      "South Sudan",
      "Somalia"
    ),
    year(date) >= 2018
  ) %>%
  rowwise() %>%
  mutate(
    total_flags = sum(
      c_across(
        c(
          first_displacement_in_year:flag_yearly,
          flag_5k_day:flag_500k_yearly
        )
      )
    )
  ) %>%
  ungroup()

##############################
#### TOTAL FLAGS ANALYSIS ####
##############################

# look at how total flags compares across the various datasets

df_flags <- df_filtered %>%
  pivot_longer(
    daily:yearly
  ) %>%
  mutate(
    name = factor(
      name,
      levels = c(
        "daily",
        "weekly",
        "monthly",
        "quarterly",
        "yearly"
      )
    )
  ) %>%
  filter(
    total_flags > 0
  )

df_filtered_long <- df_filtered %>%
  pivot_longer(
    daily:yearly
  ) %>%
  mutate(
    name = factor(
      name,
      levels = c(
        "daily",
        "weekly",
        "monthly",
        "quarterly",
        "yearly"
      )
    )
  ) 

df_filtered_long %>%
  ggplot(
    aes(
    x = date,
    y = value
  )
) +
  geom_line(
    alpha = 0.2,
    lwd = 1
  ) +
  geom_point(
    data = df_flags,
    aes(
      color = total_flags
    )
  ) +
  facet_wrap(
    name~country,
    scales = "free_y"
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_color_gradient_hdx_tomato() +
  labs(
    title = "Displacement in sample countries, 2018 - 2022",
    subtitle = "Flags calculated across different time scales and thresholds",
    x = "",
    y = "",
    color = "Total #\nof flags"
  ) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y"
  ) 

###################################
#### CREATE "CONNECTED" EVENTS ####
###################################

df_flags_group <- df_filtered %>%
  filter(
    total_flags > 0
  ) %>%
  group_by(
    country
  ) %>%
  mutate(
    date_group = cumsum(date - lag(date, default = min(date)) != 1)
  ) %>%
  group_by(
    country,
    date_group
  ) %>%
  summarize(
    date_min = min(date),
    date_max = max(date)
  )

df_filtered_long %>%
  ggplot() +
  geom_rect(
    data = df_flags_group,
    aes(
      xmin = date_min,
      xmax = date_max
    ),
    ymin = 0,
    ymax = Inf,
    fill = hdx_hex("tomato-ultra-light")
  ) +
  geom_line(
    aes(
      x = date,
      y = value
    ),
    alpha = 1,
    color = hdx_hex("gray-medium"),
    lwd = 1
  ) +
  facet_wrap(
    name~country,
    scales = "free_y"
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_color_gradient_hdx_tomato() +
  labs(
    title = "Displacement in sample countries, 2018 - 2022",
    subtitle = paste(
      "Flags calculated across different time scales and thresholds,",
      "with dates flagged colored in red"
    ),
    x = "",
    y = "",
    color = "Total #\nof flags"
  ) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y"
  ) 

ggsave(
  here(
    plot_dir,
    "flags-area.png"
  ),
  width = 15,
  height = 10,
  units = "in"
)
