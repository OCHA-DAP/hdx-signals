# Script to analyse the cholera data from WHO

# by country and checking for common indicators of an uptick in cases
options(scipen = 999)
# load libraries

box::use(
  readr,
  dplyr,
  purrr,
  tidyr,
  lubridate,
  janitor,
  stringr,
  zoo,
  ggplot2,
  gghdx,
  scales
)
box::use(src/utils/location_codes)
gghdx$gghdx()
df_raw_link <- "https://raw.githubusercontent.com/CBPFGMS/pfbi-data/refs/heads/main/final_data_for_powerbi_with_kpi.csv"

# read in the data
df_raw <- readr$read_csv(df_raw_link)
names(df_raw)

# cerf allocations from HDX
df_cerf <- readr$read_csv(file.path(Sys.getenv("AA_DATA_DIR"),
                          "public", "raw", "glb", "cerf_allocations", "Data_ CERF Donor Contributions and Allocations - allocations (1).csv"))
cerf_cholera <- df_cerf |>
  dplyr$filter(if_any(everything(), ~ grepl("cholera", ., ignore.case = TRUE)))


# there are 3 columns with start of reporting period
# checking if the columns with `Start of reporting period` are the same
table(df_raw$`Start of reporting period` == df_raw$`Start of reporting
period`)

table(df_raw$`Start of reporting period` == df_raw$`Start of
reporting period`)

# it seems these columns are reporting on the same value and if one has a value, the rest are NA.

df_clean <- df_raw |>
  # Clean column names to snake_case using janitor
  janitor$clean_names() |>

  # Filter for rows where the event contains 'cholera' but exclude other similar diseases
  dplyr$filter(
    stringr$str_detect(tolower(event), "cholera") &
      !stringr$str_detect(tolower(event), "intestinal|bacterial|shigellosis|salmonellosis")
  ) |>

  # Select and rename key columns
  dplyr$transmute(
    iso3 = location_codes$names_to_iso3(country),  # Convert country names to ISO3 codes
    event,

    # Consolidate multiple possible start date fields
    start_date_raw = dplyr$case_when(
      !is.na(start_of_reporting_period) ~ start_of_reporting_period,
      !is.na(start_of_reporting_period_2) ~ start_of_reporting_period_2,
      !is.na(start_of_reporting_period_3) ~ start_of_reporting_period_3
    ),

    date = as.Date(week_date),  # Parse the week_date column to Date format
    #week_date_start = date - lubridate$wday(date, week_start = 1) + 1,  # Calculate the start of the week

    # Parse start_date using appropriate format based on date patterns or time
    start_date = dplyr$case_when(
      stringr$str_detect(start_date_raw, "[0-9]{1,2}[-|//][A-Za-z]{3}") ~ lubridate$dmy(start_date_raw, quiet = TRUE),
      stringr$str_detect(start_date_raw, "^[A-Za-z]{3}") ~ lubridate$mdy(start_date_raw, quiet = TRUE),
      date >= "2023-09-25" ~ lubridate$mdy(start_date_raw, quiet = TRUE),
      date < "2023-09-25" ~ lubridate$dmy(start_date_raw, quiet = TRUE)
    ),

    cholera_cases = readr$parse_number(as.character(total_cases))  # Extract numeric case count
  ) |>

  # Drop the raw start_date column
  dplyr$select(-start_date_raw) |>

  # Group by ISO3 and date for aggregation
  dplyr$group_by(iso3, date) |>

  # Summarize by collapsing multiple event descriptions and summing cases
  dplyr$summarize(
    event = paste(unique(event), collapse = "; "),
    start_date = min(start_date),
    cholera_cases = sum(cholera_cases),
    .groups = "drop"
  ) |>

  # Re-group by country
  dplyr$group_by(iso3) |>

  # Sort by date within each group
  dplyr$arrange(date, .by_group = TRUE) |>

  # Interpolate missing case values using linear approximation
  dplyr$mutate(
    cholera_cases = zoo$na.approx(cholera_cases, na.rm = FALSE)
  ) |>
  # Fill in missing dates with NA values for cholera cases
  #tidyr$complete(week_date_start = seq(min(week_date_start), max(week_date_start), by = "1 week")) |>
  # Cleaning up values shown as decimals

  # Remove grouping
  dplyr$ungroup()

## First, check if values tend to be increasing (cumulative) for each country
df_rising_check <- df_clean |>
  dplyr$group_by(iso3) |>
  dplyr$arrange(iso3, date) |>
  dplyr$mutate(
    rising_cases = cholera_cases >= dplyr$lag(cholera_cases, 1, default = NA),
  ) |>
  dplyr$summarise(mean(rising_cases, na.rm=T))

# most of the values are greater than or equal to the one prior to it. (lowest being close to 90%)
# giving a strong indication of cumulative number of cases.

## Next, some data cleaning
# one thing to note is that these weekdays for the date change
# these dates change depending on the year and weeks can change even within the year.
# tends to be between Mon-Wed
# tried to fill in values and seems there is an issue with the definition of weeks
# Replace values of 1 and
# those with decimals especially those around other larger values
# Fill in the missing weeks.
decimal_values <- df_clean |>
  dplyr$filter(cholera_cases %% 1 != 0)

small_values <- df_clean |>
  dplyr$filter(cholera_cases < 200)

df_prepped <- df_clean |>
  #dplyr$mutate(year = lubridate$year(date)) |>
  dplyr$group_by(iso3) |>
  #tidyr$complete(date = seq(min(date), max(date), by = "1 week")) |>
  dplyr$mutate(
    original_value = cholera_cases,
    # if previous value is more than 3 weeks ago, do not change small values
    prev_date_diff = as.numeric(date - dplyr$lag(date, 1, default = NA)),
    # Create the previous and next value columns
    # Get the previous values
    prev_val = pmax(
      dplyr$lag(cholera_cases, 1, default = NA),
      dplyr$lag(cholera_cases, 2, default = NA),
      dplyr$lag(cholera_cases, 3, default = NA),
      dplyr$lag(cholera_cases, 4, default = NA),
      dplyr$lag(cholera_cases, 5, default = NA),
      na.rm = TRUE
    ),
    # Get the next values
    next_val = pmax(
      dplyr$lead(cholera_cases, 1, default = NA),
      dplyr$lead(cholera_cases, 2, default = NA),
      dplyr$lead(cholera_cases, 3, default = NA),
      dplyr$lead(cholera_cases, 4, default = NA),
      dplyr$lead(cholera_cases, 5, default = NA),
      na.rm = TRUE
    ),
    # Calculate the reference value as the larger of the previous and next values
    # ref = dplyr$if_else(!is.na(prev_val) & !is.na(next_val), pmax(prev_val, next_val), NA_real_),
    # Calculate the scaled cholera_cases (multiplying by 10, 100, and 1000)
    scaled_1 = cholera_cases * 1,
    scaled_10 = cholera_cases * 10,
    scaled_100 = cholera_cases * 100,
    scaled_1000 = cholera_cases * 1000,
    scaled_10000 = cholera_cases * 10000,
    # Calculate the absolute differences between scaled values and the reference
    diffs_prev_1 = abs(scaled_1 - prev_val),
    diffs_prev_10 = abs(scaled_10 - prev_val),
    diffs_prev_100 = abs(scaled_100 - prev_val),
    diffs_prev_1000 = abs(scaled_1000 - prev_val),
    diffs_prev_10000 = abs(scaled_10000 - prev_val),
    diffs_next_1 = abs(scaled_1 - next_val),
    diffs_next_10 = abs(scaled_10 - next_val),
    diffs_next_100 = abs(scaled_100 - next_val),
    diffs_next_1000 = abs(scaled_1000 - next_val),
    diffs_next_10000 = abs(scaled_10000 - next_val),
    # Find the minimum difference for each row
    min_diff = pmin(diffs_prev_1, diffs_prev_10, diffs_prev_100, diffs_prev_1000, diffs_prev_10000, diffs_next_1, diffs_next_10, diffs_next_100, diffs_next_1000, diffs_next_10000, na.rm = TRUE),
    # Find the best match by selecting the scaled value with the smallest difference
    best_match = floor(dplyr$case_when(
      min_diff == diffs_prev_1 ~ scaled_1,
      min_diff == diffs_prev_10 ~ scaled_10,
      min_diff == diffs_prev_100 ~ scaled_100,
      min_diff == diffs_prev_1000 ~ scaled_1000,
      min_diff == diffs_prev_10000 ~ scaled_10000,
      min_diff == diffs_next_1 ~ scaled_1,
      min_diff == diffs_next_10 ~ scaled_10,
      min_diff == diffs_next_100 ~ scaled_100,
      min_diff == diffs_next_1000 ~ scaled_1000,
      min_diff == diffs_next_10000 ~ scaled_10000,
      TRUE ~ cholera_cases
    )),
    best_match_class = ceiling(best_match / cholera_cases),
    ref = dplyr$case_when(
      is.na(prev_val) & is.na(next_val) ~ NA_real_,
      is.na(prev_val) ~ next_val,
      is.na(next_val) ~ prev_val,
      abs(prev_val - best_match) <= abs(next_val - best_match) ~ prev_val,
      TRUE ~ next_val
    ),
    # Apply the scaling logic: update cholera_cases based on the best match
    cholera_cases = dplyr$case_when(
      # prev_value == next value == current value, then keep current value
      (dplyr$lag(cholera_cases, 1) == dplyr$lead(cholera_cases, 1) & dplyr$lag(cholera_cases, 1) == cholera_cases) ~ cholera_cases,
      # If the value is non-integer and ref exists, check min_diff against best_match_class
      ((cholera_cases %% 1 != 0 & !is.na(ref))) ~ {
        dplyr$if_else((min_diff < 1 | min_diff < best_match_class | best_match_class >= 100), best_match, floor(cholera_cases))
      },
      # If cholera_cases is small (< 200), and strong match to prev/next
      cholera_cases < 200 & !is.na(ref) ~ {
        dplyr$if_else((prev_val == next_val | min_diff < best_match_class) &
                        (!is.na(prev_val) & !is.na(next_val)) &
                        (prev_date_diff <= 28), ref, cholera_cases)
      },
      # If min_diff is smaller than best_match_class, update
      min_diff < best_match_class & !is.na(ref) ~ {
        best_match
      },
      TRUE ~ floor(cholera_cases)
    )
  ) |>
  dplyr$select(iso3, date, event, start_date, cholera_cases, prev_date_diff)

# quick check on how many weeks per country and year in dataset
df_weeks_per_country <- df_prepped |>
  dplyr$mutate(year = lubridate$year(date)) |>
  dplyr$group_by(iso3, year) |>
  dplyr$summarize(weeks = dplyr$n(), .groups = "drop") |>
  dplyr$ungroup() |>
  dplyr$group_by(iso3) |>
  dplyr$summarize(min_weeks = min(weeks), max_weeks = max(weeks), mean_weeks = mean(weeks), .groups = "drop")

# check for countries where dates are less than 7 days apart
df_prepped |>
  dplyr$filter(prev_date_diff < 7) |>
  dplyr$group_by(iso3) |>
  dplyr$summarise(count = dplyr$n())

# Global Cholera Trigger Options:

# 1. Use the DRC trigger:
#    - Level 3 alert sustained for 3 weeks
#    - Level 3 defined as either:
#        a) 4x increase compared to baseline, or
#        b) Value above the 99th percentile

# 2. Use the old cumulative methodology (as included by Seth in the Signals system)

# 3. Use the Signals methodology:
#    - Alert when current value is higher than any point in the past 12 months

# Function to find nearest date match for each country
find_nearest_country <- function(event_df, allocation_df) {
  event_df |>
    dplyr$group_by(iso3) |>  # Group by country
    dplyr$rowwise() |>
    dplyr$mutate(
      # pull only the allocation dates for this same iso3
      country_alloc_dates = list(allocation_df$dateUSGSignature[allocation_df$countryCode == iso3]),
      # pick the one closest to this event’s date
      # if no dates, give NA; otherwise pick the one with the smallest abs diff
      nearest_allocation_date = if (length(country_alloc_dates) == 0) {
        NA
      } else {
        country_alloc_dates[which.min(abs(difftime(date, country_alloc_dates, units = "days")))]
      },
      # compute signed or absolute difference as you wish
      time_diff_days = as.numeric(difftime(nearest_allocation_date, date, units = "days"))
    ) |>
    dplyr$ungroup()
}
allocation_years <- cerf_cholera |>
  dplyr$filter(!is.na(dateUSGSignature)) |>
  dplyr$mutate(year = lubridate$year(dateUSGSignature)) |>
  dplyr$distinct(countryCode, year) |>
  dplyr$filter(year >= lubridate$year(min(df_prepped$date, na.rm = T))) |>
  dplyr$rename(iso3 = countryCode)
# Methodology 1
# Assumption: Values stay the same across weeks without data until reported otherwise
# Also making the data noncumulative
df_1 <- df_prepped |>
  dplyr$group_by(iso3) |>
  dplyr$mutate(
    weekly_increase = dplyr$if_else(prev_date_diff <= 21,
                                    cholera_cases - dplyr$lag(cholera_cases, 1),
                                    NA_real_),
    p95 = quantile(weekly_increase, probs = 0.95, na.rm = TRUE),
    p975 = quantile(weekly_increase, probs = 0.975, na.rm = TRUE),
    p99 = quantile(weekly_increase, probs = 0.99, na.rm = TRUE),
    year = lubridate$year(date),
    alert_level = dplyr$case_when(
      weekly_increase > p99 ~ "p99",
      weekly_increase > p975 ~ "p975",
      weekly_increase > p95 ~ "p95",
      TRUE ~ "none"
    )
  ) |>
  dplyr$ungroup()

# defining levels, 95 as baseline, 97.5 as watch and 99 as alert
# testing how often each is reached
# Summarize the number of alerts by country, year, and alert level
alerts_by_year_and_level <- df_1 |>
  dplyr$count(iso3, year, alert_level, name = "num_alerts") |>
  dplyr$filter(alert_level != "none")

# Plot the heatmap of the number of alerts by country, year, and alert level
ggplot2$ggplot(alerts_by_year_and_level, ggplot2$aes(x = year, y = iso3, fill = num_alerts)) +
  ggplot2$geom_tile() +  # Use tiles to create a heatmap
  ggplot2$geom_tile(
    data = allocation_years,
    mapping = ggplot2$aes(x = year, y = iso3),
    fill = NA,
    color = "darkgreen",
    size = 0.55
  ) +
  ggplot2$geom_text(ggplot2$aes(label = num_alerts), color = "black", size = 3) +
  ggplot2$labs(
    title = "Number of Alerts by Country, Year, and Alert Level",
    x = "Year",
    y = "Country",
    fill = "Number of Alerts",
    caption = "Green outlines indicate years with a CERF allocation"
  ) +
  ggplot2$facet_wrap(~alert_level) +  # Facet by alert level to create separate tiles for each level
  ggplot2$scale_fill_gradient2(low = "white", high = "red",
                               breaks = function(x) x[x %% 1 == 0],
                               labels = function(x) x[x %% 1 == 0]) +  # Adjust labels
  ggplot2$scale_y_discrete(limits = rev(levels(factor(alerts_by_year_and_level$iso3)))) +  # Alphabetical ordering of countries
  ggplot2$scale_x_continuous(breaks = unique(alerts_by_year_and_level$year))


# comparing with CERF allocations
# which alert level correlates with a CERF allocation
df_1_cerf <- find_nearest_country(event_df=df_1, allocation_df=cerf_cholera)
df_summary <- df_1_cerf |>
  # baseline of 1000
  dplyr$mutate(
    n_window = time_diff_days >= -30 & time_diff_days <= 90
  ) |>
  dplyr$filter(weekly_increase >= 1000 & alert_level != "none" & n_window)
df_summary |>
  dplyr$group_by(alert_level) |>
  dplyr$summarise(
    n = dplyr$n(),
    n_window = sum(n_window, na.rm = TRUE),
    prop_window = n_window / n,
    .groups = "drop"
  )


# Methodology 2
# This method should use cumulative values.
df_2_a <- df_prepped |>
  dplyr$arrange(iso3, date) |>
  dplyr$mutate(
    weekly_increase = dplyr$if_else(prev_date_diff <= 21, cholera_cases - dplyr$lag(cholera_cases, 1), NA_real_),
    level = dplyr$case_when(
      weekly_increase > 5000 & dplyr$lag(weekly_increase, default = 0) <= 5000 ~ "Level 2",
      weekly_increase > 1000 & dplyr$lag(weekly_increase, default = 0) <= 1000 ~ "Level 1",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr$filter(!is.na(level))

# how many alerts are generated by country
# Calculate the number of alerts per year for each country
alerts_by_year <- df_2_a |>
  dplyr$mutate(year = lubridate$year(date)) |>
  dplyr$count(iso3, year, level, name = "num_alerts") |>
  dplyr$arrange(iso3)

# Plot a box plot of the number of alerts per year by country
# Create a heatmap to visualize the number of alerts by country and year
ggplot2$ggplot(alerts_by_year, ggplot2$aes(x = year, y = iso3, fill = num_alerts)) +
  ggplot2$geom_tile() +  # Use tiles to create a heatmap
  ggplot2$geom_tile(
    data = allocation_years,
    mapping = ggplot2$aes(x = year, y = iso3),
    fill = NA,
    color = "darkgreen",
    size = 0.55
  ) +
  ggplot2$geom_text(ggplot2$aes(label = num_alerts), color = "black", size = 3) +
  ggplot2$labs(
    title = "Number of Alerts by Country, Year, and Alert Level",
    x = "Year",
    y = "Country",
    fill = "Number of Alerts",
    caption = "Green outlines indicate years with a CERF allocation"
  ) +
  ggplot2$facet_wrap(~level) +
  ggplot2$scale_fill_gradient(low = "white", high = "red",
                               breaks = function(x) x[x %% 1 == 0],
                               labels = function(x) x[x %% 1 == 0]) +
  ggplot2$scale_y_discrete(limits = rev(levels(factor(alerts_by_year$iso3)))) +
  ggplot2$scale_x_continuous(breaks = unique(alerts_by_year$year))

# comparing with CERF allocations
df_2a_cerf <- find_nearest_country(event_df=df_2_a, allocation_df=cerf_cholera)
df_2a_cerf |>
  dplyr$summarise(
    n=dplyr$n(),
    n_window=sum(time_diff_days >= -60 & time_diff_days <= 90, na.rm=T),
    prop_window = n_window / n,
  )

#df_2_b <- df_prepped |>
#  dplyr$arrange(iso3, date) |>
#  dplyr$mutate(
#    level = dplyr$case_when(
#      cholera_cases > 5000 & dplyr$lag(cholera_cases, default = 0) <= 5000 ~ "Level 2",
#      cholera_cases > 1000 & dplyr$lag(cholera_cases, default = 0) <= 1000 ~ "Level 1",
#      TRUE ~ NA_character_
#    )
#  ) |>
#  dplyr$filter(!is.na(level))

# comparing with CERF allocations
#df_2b_cerf <- find_nearest_country(event_df=df_2_b, allocation_df=cerf_cholera)
#df_2b_cerf |>
#  dplyr$summarise(
#    n=dplyr$n(),
#    n_window=sum(time_diff_days >= -60 & time_diff_days <= 90, na.rm=T),
#    prop_window = n_window / n,
#  )
# Methodology 3
# remove zeros from weekly increases
# adding check for no signals for the next 3 months
df_3 <- df_prepped |>
  dplyr$group_by(iso3) |>
  dplyr$mutate(
    weekly_increase = dplyr$if_else(prev_date_diff <= 21, cholera_cases - dplyr$lag(cholera_cases, 1), 0),
    alert_initial = purrr$map_lgl(date, ~{
      window_start <- .x - months(12)
      window_vals <- weekly_increase[date > window_start & date <= .x]

      if (all(is.na(window_vals))) {
        FALSE
      } else {
        current_val <- weekly_increase[date == .x]
        max_val <- max(window_vals, na.rm = TRUE)
        current_val == max_val
      }
    }),
    alert = purrr$map2_lgl(date, seq_along(date), ~{
      if (!alert_initial[.y] || weekly_increase[.y] < 100) return(FALSE)

      last_alert_date <- max(date[alert_initial & date < .x], na.rm = TRUE)

      if (!is.finite(last_alert_date)) {
        TRUE
      } else {
        difftime(.x, last_alert_date, units = "days") > 90
      }
    })
  ) |>
  dplyr$filter(alert)


# comparing with CERF allocations
df_3_cerf <- find_nearest_country(event_df=df_3, allocation_df=cerf_cholera)
# Aggregating the data for CERF allocation windows
df_3_aggregated <- df_3_cerf |>
  dplyr$group_by(iso3, year = lubridate$year(date)) |>
  dplyr$count(alert, name = "num_alerts") |>
  dplyr$ungroup()


# plotting the heat map
ggplot2$ggplot(df_3_aggregated, ggplot2$aes(x = year, y = iso3, fill = num_alerts)) +
  ggplot2$geom_tile() +
  ggplot2$geom_tile(
    data = allocation_years,
    mapping = ggplot2$aes(x = year, y = iso3),
    fill = NA,
    color = "darkgreen",
    size = 0.55
  ) +
  ggplot2$geom_text(ggplot2$aes(label = num_alerts), color = "black", size = 3) +
  ggplot2$labs(
    title = "Number of Alerts by Country and Year",
    x = "Year",
    y = "Country",
    fill = "Number of Alerts",
    caption = "Green outlines indicate years with a CERF allocation"
  ) +
  ggplot2$scale_fill_gradient(low = "white", high = "red",
                              breaks = function(x) x[x %% 1 == 0],
                              labels = function(x) x[x %% 1 == 0]) +
  ggplot2$scale_y_discrete(limits = rev(levels(factor(df_3_aggregated$iso3)))) +
  ggplot2$scale_x_continuous(breaks = unique(df_3_aggregated$year))


## a time series of cases by country showing the alert levels

# Combine
df_joined <- dplyr$full_join(df_1, df_2_a,
                             by = c("iso3", "date"),
                             suffix = c("_df1", "_df2a")) |>
  dplyr$full_join(df_3, by = c("iso3", "date"))

# Plot

iso3_levels <- unique(combined_df$iso3)
purrr$walk(iso3_levels, ~ {
  print({
    ggplot2$ggplot(combined_df[combined_df$iso3 == .x, ], ggplot2$aes(x = date, y = weekly_increase)) +
      ggplot2$geom_line() +
      ggplot2$geom_point(data = subset(combined_df, iso3 == .x & alert_level == "p99"),
                         ggplot2$aes(x = date, y = weekly_increase),
                         color = "red", size = 2) +
      ggplot2$geom_point(data = subset(combined_df, iso3 == .x & level == "Level 2"),
                         ggplot2$aes(x = date, y = weekly_increase),
                         color = "orange", size = 2) +
      ggplot2$geom_point(data = subset(combined_df, iso3 == .x & alert == TRUE),
                         ggplot2$aes(x = date, y = weekly_increase),
                         color = "#0F52BA", size = 2) +
      ggplot2$geom_hline(ggplot2$aes(yintercept = p99), color = "red", linetype = "dashed", show.legend = FALSE) +
      ggplot2$geom_hline(yintercept = 1000, color = "darkgreen", linetype = "dashed", show.legend = FALSE) +
      ggplot2$geom_hline(yintercept = 5000, color = "orange", linetype = "dashed", show.legend = FALSE) +
      ggplot2$annotate("text", x = as.Date("2021-01-01"), y = 1000, label = "1000", color = "darkgreen", hjust = 0, vjust = -0.5) +
      ggplot2$annotate("text", x = as.Date("2022-01-01"), y = 5000, label = "5000", color = "orange", hjust = 0, vjust = -0.5) +
      ggplot2$geom_text(ggplot2$aes(x = as.Date("2020-01-01"), y = p99, label = "p99"), color = "red", hjust = 0, vjust = -0.5) +
      ggplot2$labs(
        title = paste("Weekly Increase in Cholera Cases:", .x),
        x = "Date",
        y = "Weekly Increase",
        caption = "red dots show alerts for method 1 \norange dots show alerts for method 2 \nblue dots show an alert for method 3"
      ) +
      gghdx$gghdx()
  })
})
