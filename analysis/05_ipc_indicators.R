## Compute indicators of trends in food security situation

##########
# set up
##########

# remove scientific notation
options(scipen=999)

## install, load libraries
pacman::p_load(tidyverse, ggplot2)

# paths
data_dir <- Sys.getenv("AA_DATA_DIR")
monitoring_data_folder <- paste0(data_dir, "/private/exploration/glb/global_monitoring")

# date variable to save in filenames
today <- Sys.Date()

##########
# data loading
##########
                               ## FIX ME WITH LINK TO HDX ONCE FILE IS UPLOADED
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
# indicators per adm0
##########

## aggregate per adm0
adm0_agg <- data %>%
              group_by(country, date_of_analysis, analysis_type) %>%
              summarise_at(c("population", "phase_1_num", "phase_2_num", "phase_3_num", "phase_4_num", "phase_5_num", "phase_p3._num", "phase_p4._num"), sum, na.rm = TRUE) %>%
              ungroup() %>%
              arrange(., country, desc(date_of_analysis), analysis_type)

## compute pct per adm0
adm0_agg <- adm0_agg %>%
            mutate(phase_1_pct = round(phase_1_num / population, 2)
                   ,phase_2_pct= round(phase_2_num / population, 2)
                   ,phase_3_pct = round(phase_3_num / population, 2)
                   ,phase_4_pct = round(phase_4_num / population, 2)
                   ,phase_5_pct = round(phase_5_num / population, 2)
                   ,phase_p3._pct = round(phase_p3._num / population, 2)
                   ,phase_p4._pct = round(phase_p4._num / population, 2))


## create list of all adm0's
adm0_pcodes <- unique(adm0_agg$country) ## FIX ME WHEN PCODES AVAILABLE

## compute changes since last assessment: (Cur minus last cur) deltas in percentage points
cur_last_deltas <- data.frame()

for (pcode in adm0_pcodes) {

output <- adm0_agg %>%
  filter(country == pcode & analysis_type == "current") %>%
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
  adm0_df <- adm0_agg %>%
    filter(country == pcode)

    for (ind in indicators) {
    output <- adm0_df %>%
      select(date_of_analysis, analysis_type, (!!as.name(ind))) %>% # dynamically select indicator column
      pivot_wider(names_from = analysis_type,
                  values_from = (!!as.name(ind))) %>%
      mutate(proj3_cur_delta = first_projection - current) %>%
      pivot_longer(!date_of_analysis, names_to = "indicator", values_to = "value") %>%
      mutate(indicator = paste0(ind, "_", indicator),
             adm0_pcode = pcode)

    proj3_cur_deltas <- rbind(proj3_cur_deltas, output)

    }
}

proj3_cur_deltas_wide <- proj3_cur_deltas %>%
                          pivot_wider(names_from = indicator,
                                      values_from = value)

##########
# alerts: verify if alert thresholds are met
##########

# create dataframe to receive alerts info
alerts <- data.frame()

# Alert1: Population in IPC3+ is greater than 0 when last assessment was zero
alert1 <- cur_last_deltas %>%
  filter(prev_phase3p_pct == 0 &
         phase_p3._pct > 0) %>%
  select(country, date_of_analysis, phase_p3._pct) %>%
  rename(value = phase_p3._pct) %>%
  add_column(alert = "alert1",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert1)

# Alert2: Population in IPC3+ projected greater than 0 when current assessment is zero
alert2 <- proj3_cur_deltas_wide %>%
  filter(phase_p3._pct_first_projection > 0 &
           phase_p3._pct_current == 0) %>%
  select(adm0_pcode, date_of_analysis, phase_p3._pct_first_projection) %>%
  rename(country = adm0_pcode,
         value = phase_p3._pct_first_projection) %>% ## FIX ME
  add_column(alert = "alert2",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert2)

# Alert3: Population in IPC4+ is greater than 0 when last assessment was zero
alert3 <- cur_last_deltas %>%
  filter(prev_phase4p_pct == 0 &
           phase_p4._pct > 0) %>%
  select(country, date_of_analysis, phase_p4._pct) %>%
  rename(value = phase_p4._pct) %>%
  add_column(alert = "alert3",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert3)

# Alert4: Population in IPC4+ projected to be greater than 0 when currently it is zero
alert4 <- proj3_cur_deltas_wide %>%
  filter(phase_p4._pct_first_projection > 0 &
           phase_p4._pct_current == 0) %>%
  select(adm0_pcode, date_of_analysis, phase_p4._pct_first_projection) %>%
  rename(country = adm0_pcode,
         value = phase_p4._pct_first_projection) %>% ## FIX ME
  add_column(alert = "alert4",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert4)

# Alert5: Population in IPC5 is greater than 0 when last assessment was zero
alert5 <- cur_last_deltas %>%
  filter(prev_phase5_pct == 0 &
           phase_5_pct > 0) %>%
  select(country, date_of_analysis, phase_5_pct) %>%
  rename(value = phase_5_pct) %>%
  add_column(alert = "alert5",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert5)

# Alert6: Population in IPC5 projected to be greater than 0 when currently it is zero
alert6 <- proj3_cur_deltas_wide %>%
  filter(phase_5_pct_first_projection > 0 &
         phase_5_pct_current == 0) %>%
  select(adm0_pcode, date_of_analysis, phase_5_pct_first_projection) %>%
  rename(country = adm0_pcode,
         value = phase_5_pct_first_projection) %>% ## FIX ME
  add_column(alert = "alert6",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert6)

# Alert7: Delta in IPC 4+ is greater than 0 (current minus last)
alert7 <- cur_last_deltas %>%
  filter(cur_prev_delta_phase4p_pct > 0) %>%
  select(country, date_of_analysis, cur_prev_delta_phase4p_pct) %>%
  rename(value = cur_prev_delta_phase4p_pct) %>%
  add_column(alert = "alert7",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert7)

# Alert8: Delta in IPC 5 is greater than 0 (current minus last)
alert8 <- cur_last_deltas %>%
  filter(cur_prev_delta_phase5_pct > 0) %>%
  select(country, date_of_analysis, cur_prev_delta_phase5_pct) %>%
  rename(value = cur_prev_delta_phase5_pct) %>%
  add_column(alert = "alert8",
             .after = "date_of_analysis")

alerts <- rbind(alerts, alert8)

# Alert9: Delta in IPC 4+ is greater than 0 (projected minus current)
# Alert10: Delta in IPC 5 is greater than 0 (projected minus current)





## save outputs
write.csv(cur_last_deltas, file = paste0(monitoring_data_folder, "/outputs/", today, "_cur_last_deltas.csv"))
write.csv(proj3_cur_deltas_wide, file = paste0(monitoring_data_folder, "/outputs/", today, "_proj3_cur_deltas.csv"))


