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
df <- dplyr$bind_rows(
  dplyr$tibble( # IDMC displacement conflict
    indicator_id = "idmc_displacement_conflict",
    mc_interest = "Conflict-driven displacement - IDMC",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - IDMC Conflict",
    indicator_subject = "Conflict-driven displacement",
    static_segment = 25093,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/9f24c42f-e363-9843-0789-c5a2ef4c1e94.png", #nolint
    data_source = "Displacement data - IDMC"
  ),
  dplyr$tibble( # IDMC displacement disaster
    indicator_id = "idmc_displacement_disaster",
    mc_interest = "Disaster-driven displacement - IDMC",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - IDMC Disaster",
    indicator_subject = "Disaster-driven displacement",
    static_segment = 25097,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/9f24c42f-e363-9843-0789-c5a2ef4c1e94.png", #nolint
    data_source = "Displacement data - IDMC"
  ),
  dplyr$tibble( # IPC food insecurity
    indicator_id = "ipc_food_insecurity",
    mc_interest = "Food insecurity - IPC/CH",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - IPC",
    indicator_subject = "Food insecurity",
    static_segment = 25101,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/acc34c0e-ad04-3e80-a1a0-1c9a4a664926.png", #nolint
    data_source = "Food insecurity - IPC/CH"
  ),
  dplyr$tibble( # WHO cholera
    indicator_id = "who_cholera",
    mc_interest = NA_character_,
    mc_tag = "CHOLERA",
    mc_folder = "HDX Signals - Cholera",
    indicator_subject = "Cholera",
    static_segment = 25105,
    banner_url = NA_character_,
    data_source = "Cholera data - WHO"
  ),
  dplyr$tibble( # JRC agricultural hotspots
    indicator_id = "jrc_agricultural_hotspots",
    mc_interest = "Agricultural hotspots - JRC ASAP",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - JRC ASAP",
    indicator_subject = "Agricultural hotspots",
    static_segment = 25109,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/c13c0ad9-5a19-6bf1-b759-9cb83b37905e.png", #nolint
    data_source = "Agricultural hotspots - JRC ASAP"
  ),
  dplyr$tibble( # WFP market monitoring
    indicator_id = "wfp_market_monitor",
    mc_interest = "Market monitoring - WFP",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - WFP Markets",
    indicator_subject = "Market monitoring",
    static_segment = 25113,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/91c6f44c-dcce-3982-406a-0316c1bd30ec.png", #nolint
    data_source = "Market monitoring - WFP"
  ),
  dplyr$tibble( # ACLED conflict
    indicator_id = "acled_conflict",
    mc_interest = "Armed conflict - ACLED",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - ACLED",
    indicator_subject = "Armed conflict",
    static_segment = 25117,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/3ad15370-6636-586a-b8c2-c49297531694.png", #nolint
    data_source = "Armed conflict - ACLED"
  )
)

fname <- "input/indicator_mapping.parquet"
cs$update_az_file(
  df = df,
  name = fname
)

log_info(paste0("Successfully created indicator mapping and saved to ", fname))
