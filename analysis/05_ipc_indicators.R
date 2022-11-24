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

# Current food security levels
Nbr of people and percentage of population in IPC3, IPC4, IPC5, IPC3+, IPC4+
  Previous food security levels
Nbr of people and percentage of population in IPC3, IPC4, IPC5, IPC3+, IPC4+

  Indicators
Change in food security levels since last assessment
Delta of number of people in IPC3, IPC4, IPC5, IPC3+, IPC4+ (count, percentage change)
Delta of percentage of population in IPC3, IPC4, IPC5, IPC3+, IPC4+ (percentage points, percentage change)
Projected change in food security levels (3 and 6 months out)
Delta of number of people in IPC3, IPC4, IPC5, IPC3+, IPC4+ (count, percentage change)
Delta of percentage of population in IPC3, IPC4, IPC5, IPC3+, IPC4+ (percentage points, percentage change)

