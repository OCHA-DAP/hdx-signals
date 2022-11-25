## Compute metrics on changes in food security situation

##########
# set up
##########

## install, load libraries
pacman::p_load(tidyverse, ggplot2)

## load data ## FIX ME WITH LINK TO HDX ONCE FILE IS UPLOADED
data <- read.csv("ipc_complete_data.csv")

##########
# data formatting
##########

## format date fields
data$date_of_analysis <- as.Date(paste0("01", data$date_of_analysis), "%d%b%Y")
data$analysis_period_start <- as.Date(data$analysis_period_start)
data$analysis_period_end <- as.Date(data$analysis_period_end)

## compute phase Phase 4+
data$phase_p4._num <- data$phase_4_num + data$phase_5_num
data$phase_p4._pct <- round(data$phase_p4._num / data$population, 2)

##########
# indicators
##########

# create list of all adm1's
adm1_pcodes <- unique(data$area) ## FIX ME WHEN PCODES AVAILABLE

# create empty dataframe to receive results
cur_last_deltas <- data.frame()

# compute changes since last assessment: (Cur minus last cur) delta in percentage points

for (adm1_pcode in adm1_pcodes) {

output <- data %>%
  filter(area == adm1_pcode & analysis_type == "current") %>%
  arrange(desc(date_of_analysis)) %>%
  mutate(prev_phase3p_pct = lead(phase_p3._pct),
         cur_prev_delta_phase3p_pct = phase_p3._pct - prev_phase3p_pct,
         prev_phase4p_pct = lead(phase_p4._pct),
         cur_prev_delta_phase4p_pct = phase_p4._pct - prev_phase4p_pct,
         prev_phase5_pct = lead(phase_5_pct),
         cur_prev_delta_phase5_pct = phase_5_pct - prev_phase5_pct)

  # add to a dataframe
  cur_last_deltas <- rbind(cur_last_deltas, output)
}



