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
  ~indicator_id, ~mc_interest, ~mc_tag, ~mc_folder, ~indicator_title, ~indicator_subject, ~static_segment, ~banner_url,
  "idmc_displacement_conflict", "Conflict-driven displacement - IDMC", NA_character_, "HDX Signals - IDMC Conflict", "Conflict-driven displacement", "Conflict-driven displacement", 25093,  "https://mcusercontent.com/ea3f905d50ea939780139789d/images/9f24c42f-e363-9843-0789-c5a2ef4c1e94.png",
  "idmc_displacement_disaster", "Disaster-driven displacement - IDMC", NA_character_, "HDX Signals - IDMC Disaster", "Disaster-driven displacement","Disaster-driven displacemen", 25097,  "https://mcusercontent.com/ea3f905d50ea939780139789d/images/9f24c42f-e363-9843-0789-c5a2ef4c1e94.png",
  "ipc_food_insecurity", "Food insecurity - IPC/CH", NA_character_, "HDX Signals - IPC", "Food insecurity", "Food insecurity", 25101, "https://mcusercontent.com/ea3f905d50ea939780139789d/images/acc34c0e-ad04-3e80-a1a0-1c9a4a664926.png",
  "who_cholera", NA_character_, "CHOLERA", "HDX Signals - Cholera", "Cholera", "Cholera", 25105, "https://mcusercontent.com/ea3f905d50ea939780139789d/images/acc34c0e-ad04-3e80-a1a0-1c9a4a664926.png",
  "jrc_agricultural_hotspots", "Agricultural hotspots - JRC ASAP", NA_character_, "HDX Signals - JRC ASAP", "Agricultural hotspots", "Agricultural hotspots", 25109, "https://mcusercontent.com/ea3f905d50ea939780139789d/images/c13c0ad9-5a19-6bf1-b759-9cb83b37905e.png",
  "wfp_market_monitor", "Market monitoring - WFP", NA_character_, "HDX Signals - WFP Markets", "Market monitoring", "Market monitoring", 25113, "https://mcusercontent.com/ea3f905d50ea939780139789d/images/91c6f44c-dcce-3982-406a-0316c1bd30ec.png",
  "acled_conflict", "Armed conflict - ACLED", NA_character_, "HDX Signals - ACLED", "Armed conflict", "Armed conflict", 25117, "https://mcusercontent.com/ea3f905d50ea939780139789d/images/3ad15370-6636-586a-b8c2-c49297531694.png"
)
# nolint end

fname <- "input/indicator_mapping.parquet"
cs$update_az_file(
  df = df,
  name = fname
)

log_info(paste0("Successfully created indicator mapping and saved to ", fname))
