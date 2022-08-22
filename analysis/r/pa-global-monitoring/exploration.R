library(jsonlite)
library(httr)
library(tidyverse)
library(zoo)
library(lubridate)
library(gghdx)
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

IDMC_URL <- paste0(
  "https://helix-copilot-prod-helix-media-external.s3.amazonaws.com/external-",
  "media/api-dump/idus-all/2022-08-22-08-59-07/gxvgu/idus_all.json"
)

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

df_clean_time <-  df %>%
  mutate(
    across(
      c(contains("date"), "created_at"),
      as.Date
    ),
    start_date = if_else(
      displacement_start_date <= displacement_end_date | is.na(displacement_end_date),
      displacement_start_date,
      displacement_end_date
    ),
    end_date = if_else(
      displacement_start_date <= displacement_end_date & !is.na(displacement_end_date),
      displacement_end_date,
      displacement_start_date
    )
  ) %>%
  filter( # remove few cases where both start and end not available
    !is.na(displacement_date)
  )

df_daily <- df_clean_time %>%
  rowwise() %>%
  transmute(
    country,
    displacement_type,
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
    displacement_type,
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
    displacement_type = c("Conflict", "Disaster", "Other"),
    date = seq(min(date), max(date), by = "day"),
    fill = list(
      figure = 0
    )
  ) %>%
  arrange( # ensuring data is correctly ordered for later rolling sums
    country,
    displacement_type,
    date
  )

#########################
#### AGGREGATED DATA ####
#########################

# now that we have all cases by day, can smooth to different temporal scales
# as a rolling sum

df_rolled <- df_daily_complete %>%
  group_by(
    country,
    displacement_type
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
    first_displacement_in_6_months = daily > 0 & lag(quarterly) == 0 & lag(quarterly, n = 2) == 0,
    first_displacement_in_year = daily > 0 & lag(yearly) == 0,
    flag_daily = flag_percent(daily),
    flag_weekly = flag_percent(weekly),
    flag_monthly = flag_percent(monthly),
    flag_quarterly = flag_percent(quarterly),
    flag_yearly = flag_percent(yearly),
    ts_outliers = tsoutliers_clean(x = daily, row_numbers = row_number())
  ) %>%
  ungroup() %>%
  mutate(
    flag_5k_day = daily >= 5000,
    flag_20k_week = weekly >= 20000,
    flag_100k_month = monthly >= 100000,
    flag_250k_quarter = quarterly >= 250000,
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
      "Iraq",
      "Mozambique",
      "Myanmar",
      "South Sudan",
      "Sri Lanka"
    ),
    year(date) >= 2018
  ) %>%
  group_by(
    country,
    date
  ) %>%
  summarize(
    across(
      .cols = c(
        daily:yearly
      ),
      .fns = sum,
      na.rm = TRUE
    ),
    across(
      .cols = c(
        first_displacement_in_6_months:flag_500k_yearly
      ),
      .fns = max,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    total_flags = sum(
      c_across(
        c(
          first_displacement_in_6_months:flag_yearly,
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

# dataset with only flags for visualization

df_flags <- df_filtered_long %>%
  filter(
    total_flags > 0
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
  contiguous()

p_cumulative <- df_filtered_long %>%
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
    caption = paste0(
        "Flags include first displacement in a year; daily, weekly, monthly,",
        "quarterly, and yearly displacement in 95th percentile for",
        "that country, and global thresholds for the same time periods."
    )
  ) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y"
  ) 

ggsave(
  filename = here(
    plot_dir,
    "flag_cumulative.png"
  ),
  plot = p_cumulative,
  width = 15,
  height = 10,
  units = "in"
)

##################################
#### CHECK FLAGS INDIVIDUALLY ####
##################################

# check each flag create individually
# first create plottable names for the visuals
flag_names <-
  c(
    "First displacement reported in 6 months" = "first_displacement_in_6_months",
    "First displacement reported in a year" = "first_displacement_in_year",
    "Daily displacement in 95th percentile for that country" = "flag_daily",
    "Weekly displacement in 95th percentile for that country" = "flag_weekly",
    "Monthly displacement in 95th percentile for that country" = "flag_monthly",
    "Quarterly displacement in 95th percentile for that country" = "flag_quarterly",
    "Yearly displacement in 95th percentile for that country" = "flag_yearly",
    "Time series outlier detected in country trend" = "ts_outliers",
    "Daily displacement at or above 5,000 persons" = "flag_5k_day",
    "Weekly displacement at or above 20,000 persons" = "flag_20k_week",
    "Monthly displacement at or above 100,000 persons" = "flag_100k_month",
    "Quarterly displacement at or above 250,000 persons" = "flag_250k_quarter",
    "Yearly displacement at or above 500,000 persons" = "flag_500k_yearly"
  )

iwalk(
  flag_names,
  ~ (df_filtered_long %>%
       ggplot() +
       geom_rect(
         data = filter(
           df_filtered_long,
           as.logical(!!sym(.x))
         ) %>% contiguous(),
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
         subtitle = paste("Flag:", .y),
         x = "",
         y = ""
       ) +
       scale_x_date(
         date_breaks = "2 year",
         date_labels = "%Y"
       )
  ) %>%
    ggsave(
      filename = here(
        plot_dir,
        paste0(.x, ".png")
      )
    )
)
