library(Ripc)
library(tidyverse)
library(countrycode)
library(rvest)
library(openai)

output_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "exploration",
  "glb",
  "global_monitoring",
  "outputs"
)

#############
#### IPC ####
#############

# get into country level form
df_ipc_raw <- ipc_get_population()$country

df_ipc <- df_ipc_raw %>%
  rename_with(
    .fn = ~str_replace(.x, "phase", "phase_")
  ) %>%
  rename(
    date_of_analysis = analysis_date,
    analysis_type = period,
    population = estimated_population
  ) %>%
  mutate(
    iso3 = ifelse(
      country == "LAC",
      country,
      countrycode(country, origin = "iso2c", destination = "iso3c")
    ),
    country = ifelse(
      iso3 == "LAC",
      "Latin America and the Caribbean",
      countrycode(iso3, origin = "iso3c", destination = "country.name")
    ),
    phase_4pl_num = phase_4_num + phase_5_num,
    phase_4pl_pct = phase_4_pct + phase_5_pct
  ) %>% left_join(
    ipc_get_analyses() %>%
      transmute(
        anl_id,
        analysis_url = str_replace(link, "http://www-test.fao.org/ipcinfo-portal", "https://www.ipcinfo.org")
      ),
    by = "anl_id"
  ) %>%
  relocate(analysis_url, .after = analysis_type)

# get differences between current values and the previous current value
df_cur_delta <- df_ipc %>%
  filter(
    analysis_type == "current"
  ) %>%
  group_by(
    country,
    iso3
  ) %>%
  arrange(
    date_of_analysis,
    .by_group = TRUE
  ) %>%
  mutate(
    across(
      matches("pct$"),
      ~ .x - lag(.x),
      .names = "{col}_delta"
    ),
    title_detail = str_extract(title, "(?<=[0-9]{4})(.*)$"),
    potential_incomparability = title_detail != lag(title_detail)
  ) %>%
  slice(-1) %>%
  ungroup() %>%
  select(
    anl_id,
    analysis_type,
    ends_with("delta"),
    potential_incomparability
  )

# get differences between current and projections (or first to second proj)
df_proj_delta <- df_ipc %>%
  group_by(
    country,
    iso3,
    date_of_analysis
  ) %>%
  filter(
    "projected" %in% analysis_type
  ) %>%
  arrange(
    analysis_type,
    .by_group = TRUE
  ) %>%
  mutate(
    across(
      ends_with("pct"),
      ~ .x - lag(.x),
      .names = "{col}_delta"
    )
  ) %>%
  slice(-1) %>%
  ungroup() %>%
  select(
    anl_id,
    analysis_type,
    ends_with("delta")
  )

######################
#### WRANGLE DATA ####
######################

# wrangle the data together for a single country-level IPC dataset

df_ipc_wrangled <- left_join(
  df_ipc,
  bind_rows(df_cur_delta, df_proj_delta),
  by = c("anl_id", "analysis_type")
) %>%
  mutate(
    potential_incomparability = replace_na(potential_incomparability, FALSE)
  )

#######################
#### FLAGGING DATA ####
#######################

# flag on the IPC data by looking at wherever there is a positive percent change
df_ipc_flags <- df_ipc_wrangled %>%
  filter(
    if_any(
      .cols = c(phase_3pl_pct_delta, phase_4pl_pct_delta, phase_5_pct_delta),
      .fns = ~ !is.na(.x) & .x > 0
    ) | potential_incomparability
  ) %>%
  group_by(
    country,
    iso3,
    date_of_analysis
  ) %>%
  summarize(
    start_date = min(analysis_period_start),
    end_date = max(analysis_period_end),
    message = paste(
      c(
        ifelse(
          potential_incomparability,
          "Uncertain if the difference between the current and previous analysis is due to shifting geographic focus, check the IPC for details. ",
          ""
        ),
        "Increases in populations in phase 3+ are estimated in the ",
        paste(str_replace(unique(analysis_type), "_", " "), collapse = ", "),
        ifelse(n() == 1, " analysis.", " analyses.")
      ),
      collapse = ""
    ),
    url = unique(analysis_url)[1],
    .groups = "drop"
  ) %>%
  mutate(
    flag_type = "food_security",
    flag_source = "ipc",
    .before = "start_date"
  )

######################
#### WEB SCRAPING ####
######################

# AI summarization function

ai_summarizer <- function(req, text) {
  text <- str_trunc(text, 4000, ellipsis = "...")
  req <- paste(req, text, collapse = " ")
  insistent_ai <- insistently(
    \(req) {
      create_completion(
        model = "text-davinci-003",
        prompt = req,
        max_tokens = 100
      )$choices$text
    },
    rate = rate_delay(pause = 1, max_times = 5)
  )

  insistent_ai(req)
}

# scrape the IPC URL for additional information and pass that on to the
# ChatGPT AI model for summarization.
ipc_scraper <- function(url) {
  if (!is.na(url)) {
    txt <- read_html(url) %>%
      html_nodes("._undmaptext") %>%
      html_text()
    # extract the situation report and recommendations from the scraped text
    txt <- txt[rank(-nchar(txt)) <= 2]
    txt <- str_replace_all(txt, "[\r\n\t]+", " ")

    # feed these to the AI to get a summarization
    sit_rep <- ai_summarizer(
      "Please summarize the current food security situation in 4 sentences based on the following description -->",
      txt[1]
    )
    recs <- ai_summarizer(
      "In 4 sentences, summarize the key recommendations and actions describes below -->",
      txt[2]
    )

    # send these back to the dataset
    paste(
      "Situation summary: ",
      sit_rep,
      "Recommendations summary: ",
      recs
    )

  } else {
    NA
  }
}

df_ipc_flags$summary_experimental <- map_chr(
  df_ipc_flags$url,
  ipc_scraper,
  .progress = TRUE
)

########################
#### SAVE IPC  DATA ####
########################

write_csv(
  x = df_ipc_raw,
  file = file.path(
    output_dir,
    "ipc",
    "raw.csv"
  ),
  na = ""
)

write_csv(
  x = df_ipc_wrangled,
  file = file.path(
    output_dir,
    "ipc",
    "wrangled.csv"
  ),
  na = ""
)

write_csv(
  x = df_ipc_flags,
  file = file.path(
    output_dir,
    "ipc",
    "flags.csv"
  ),
  na = ""
)
