box::use(dplyr)
box::use(logger[log_info])

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/hs_logger)

hs_logger$configure_logger()

log_info("Updating indicator mapping...")

###########################
#### STATIC GENERATION ####
###########################

# generates the file from hardcoded values that can be changed here if necessary
# nolint start
df <- dplyr$tribble(
  ~indicator_id, ~mc_interest, ~mc_tag, ~mc_folder, ~indicator_title, ~indicator_subject, ~static_segment,
  "idmc_displacement_conflict", "Conflict-driven displacement - IDMC", NA_character_, "HDX Signals - IDMC Conflict", "Conflict-driven displacement", "Conflict-driven displacement", 25093,
  "idmc_displacement_disaster", "Disaster-driven displacement - IDMC", NA_character_, "HDX Signals - IDMC Disaster", "Disaster-driven displacement","Disaster-driven displacemen", 25097,
  "ipc_food_insecurity", "Food insecurity - IPC/CH", NA_character_, "HDX Signals - IPC", "Food insecurity", "Food insecurity", 25101,
  "who_cholera", NA_character_, "CHOLERA", "HDX Signals - Cholera", "Cholera", "Cholera", 25105,
  "jrc_agricultural_hotspots", "Agricultural hotspots - JRC ASAP", NA_character_, "HDX Signals - JRC ASAP", "Agricultural hotspots", "Agricultural hotspots", 25109,
  "wfp_market_monitor", "Market monitoring - WFP", NA_character_, "HDX Signals - WFP Markets", "Market monitoring", "Market monitoring", 25113,
  "acled_conflict", "Armed conflict - ACLED", NA_character_, "HDX Signals - ACLED", "Armed conflict", "Armed conflict", 25117
)
# nolint end

fname <- "input/indicator_mapping.parquet"
cs$update_az_file(
  df = df,
  name = fname
)

log_info(paste0("Successfully created indicator mapping and saved to ", fname))
