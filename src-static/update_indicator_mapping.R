box::use(dplyr)
box::use(logger)

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/hs_logger)

hs_logger$configure_logger()

logger$log_info("Updating indicator mapping...")

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
    data_source = "Displacement data - IDMC",
    closing_text = paste(
      'The <a href="https://www.internal-displacement.org">Internal Displacement',
      "Monitoring Centre (IDMC)</a> provides high-quality data, analysis and expertise",
      "on internal displacement to inform policy and operational decisions that can",
      "improve the lives of internally displaced people worldwide and reduce",
      "the risk of future displacement. This signal uses IDMC events data reporting",
      "displacements driven by conflict."
    )
  ),
  dplyr$tibble( # IDMC displacement disaster
    indicator_id = "idmc_displacement_disaster",
    mc_interest = "Disaster-driven displacement - IDMC",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - IDMC Disaster",
    indicator_subject = "Disaster-driven displacement",
    static_segment = 25097,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/9f24c42f-e363-9843-0789-c5a2ef4c1e94.png", #nolint
    data_source = "Displacement data - IDMC",
    closing_text = paste(
      'The <a href="https://www.internal-displacement.org">Internal Displacement',
      "Monitoring Centre (IDMC)</a> provides high-quality data, analysis and expertise",
      "on internal displacement to inform policy and operational decisions that can",
      "improve the lives of internally displaced people worldwide and reduce",
      "the risk of future displacement. This signal uses IDMC events data reporting",
      "displacements driven by natural disaster or other non-conflict hazards."
    )
  ),
  dplyr$tibble( # IPC food insecurity
    indicator_id = "ipc_food_insecurity",
    mc_interest = "Food insecurity - IPC/CH",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - IPC",
    indicator_subject = "Food insecurity",
    static_segment = 25101,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/acc34c0e-ad04-3e80-a1a0-1c9a4a664926.png", #nolint
    data_source = "Food insecurity - IPC/CH",
    closing_text = paste(
      '<a href="http://cadreharmonise.org">Cadre Harmonisé (CH)</a> and',
      '<a href="https://www.ipcinfo.org">Integrated Food Security Phase Classification (IPC)</a>',
      "are analytical frameworks which synthesize indicators of food and nutrition",
      "security outcomes and the inference of contributing factors into scales and",
      "figures representing the nature and severity of crisis and implications for",
      "strategic response in food security and nutrition."
    )
  ),
  dplyr$tibble( # WHO cholera
    indicator_id = "who_cholera",
    mc_interest = NA_character_,
    mc_tag = "CHOLERA",
    mc_folder = "HDX Signals - Cholera",
    indicator_subject = "Cholera",
    static_segment = 25105,
    banner_url = NA_character_,
    data_source = "Cholera data - WHO",
    closing_text = paste(
      "The World Health Organization (WHO) weekly bulletins for the WHO African",
      "Region cover outbreaks and other emergencies across the region."
    )
  ),
  dplyr$tibble( # JRC agricultural hotspots
    indicator_id = "jrc_agricultural_hotspots",
    mc_interest = "Agricultural hotspots - JRC ASAP",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - JRC ASAP",
    indicator_subject = "Agricultural hotspots",
    static_segment = 25109,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/c13c0ad9-5a19-6bf1-b759-9cb83b37905e.png", #nolint
    data_source = "Agricultural hotspots - JRC ASAP",
    closing_text = paste(
      '<a href="https://agricultural-production-hotspots.ec.europa.eu/index.php">',
      "Anomaly Hotspots of Agricultural Production (ASAP)</a> is an online decision support",
      "system for early warning about hotspots of agricultural production anomaly",
      "(crop and rangeland), developed by the Joint Research Centre (JRC) for food security",
      "crises prevention and response planning."
    )
  ),
  dplyr$tibble( # WFP market monitoring
    indicator_id = "wfp_market_monitor",
    mc_interest = "Market monitoring - WFP",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - WFP Markets",
    indicator_subject = "Global Market Monitor",
    static_segment = 25113,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/91c6f44c-dcce-3982-406a-0316c1bd30ec.png", #nolint
    data_source = "Global Market Monitor - WFP",
    closing_text = paste(
      "The Global Market Monitor provides information on price changes for the most",
      "commonly consumed staples and their potential impacts on the cost of a basic",
      "food basket. Impacts of price changes on the cost of a basic food basket are",
      "calculated using a standardized methodology for cross-country comparability,",
      "so figures might differ from those presented in local market monitoring reports.",
      "For more details, please refer to the",
      '<a href="https://dataviz.vam.wfp.org/reports/global-coverage-global-market-monitor-mar-2024?_ga=2.216546710.1452952701.1719219215-2028703503.1710344334">', # nolint
      "Methodology & Data page</a>."
    )
  ),
  dplyr$tibble( # ACLED conflict
    indicator_id = "acled_conflict",
    mc_interest = "Armed conflict - ACLED",
    mc_tag = NA_character_,
    mc_folder = "HDX Signals - ACLED",
    indicator_subject = "Armed conflict",
    static_segment = 25117,
    banner_url = "https://mcusercontent.com/ea3f905d50ea939780139789d/images/86ba2e73-ddc6-5555-b903-6de9cfb82ec1.png", #nolint
    data_source = "Armed conflict - ACLED",
    closing_text = paste(
      '<a href="https://acleddata.com/">The Armed Conflict Location & Event Data',
      "Project (ACLED)</a> is a disaggregated",
      "data collection, analysis, and crisis mapping project. ACLED collects the",
      "dates, actors, locations, fatalities, and types of all reported political",
      "violence and protest events in more than",
      '<a href="https://acleddata.com/download/4404/">200 countries and territories',
      "</a> in real time."
    )
  )
)

fname <- "input/indicator_mapping.parquet"
cs$update_az_file(
  df = df,
  name = fname
)

log_info(paste0("Successfully created indicator mapping and saved to ", fname))
