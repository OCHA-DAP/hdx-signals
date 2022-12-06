## Compute metrics on changes in food security situation

##########
# set up
##########

## install, load libraries
pacman::p_load(tidyverse, ggplot2)

## set up paths
data_dir <- Sys.getenv("AA_DATA_DIR")
monitoring_data_folder <- paste0(data_dir, "/private/exploration/glb/global_monitoring")

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

## create list of all adm0's
adm0_pcodes <- unique(data$country) ## FIX ME WHEN PCODES AVAILABLE

## compute changes since last assessment: (Cur minus last cur) deltas in percentage points
cur_last_deltas <- data.frame()

for (pcode in adm0_pcodes) {

output <- data %>%
  filter(area == pcode & analysis_type == "current") %>%
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


## compute changes projected in 3 months: (Proj3 minus cur) deltas in percentage points
indicators <- c("phase_p3._pct", "phase_p4._pct", "phase_5_pct")

proj3_cur_deltas <- data.frame()

for (pcode in adm0_pcodes) {
  adm0_df <- data %>%
    filter(country == pcode)

    for (ind in indicators) {
    output <- adm0_df %>%
      select(date_of_analysis, analysis_type, (!!as.name(ind))) %>%
      pivot_wider(names_from = analysis_type,
                  values_from = (!!as.name(ind))) %>%
      mutate(proj3_cur_delta = first_projection - current) %>%
      pivot_longer(!date_of_analysis, names_to = "indicator", values_to = "value") %>%
      mutate(indicator = paste0(ind, "_", indicator),
             adm0_pcode = pcode)

    proj3_cur_deltas <- rbind(proj3_cur_deltas, output)

    }
}

## save outputs
today <- Sys.Date()

write.csv(cur_last_deltas, file = paste0(monitoring_data_folder, "/outputs/", today, "_cur_last_deltas.csv"))
write.csv(proj3_cur_deltas, file = paste0(monitoring_data_folder, "/outputs/", today, "_proj3_cur_deltas.csv"))


