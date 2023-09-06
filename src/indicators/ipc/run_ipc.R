library(Ripc)
library(tidyverse)
library(countrycode)
library(rvest)

source(
  file.path(
    "src",
    "utils",
    "google_sheets.R"
  )
)

source(
  file.path(
    "src",
    "utils",
    "get_country_names.R"
  )
)

source(
  file.path(
    "src",
    "utils",
    "ai_summarizer.R"
  )
)


#############
#### IPC ####
#############

# get into country level form
df_ipc_raw <- ipc_get_population()$country %>%
  filter(
    condition == "A" # only use acute for now, chronic doesn't have date info
  ) %>%
  mutate(
    iso3 = ifelse(
      country == "LAC",
      country,
      countrycode(country, origin = "iso2c", destination = "iso3c")
    )
  ) %>%
  get_country_names()

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
  relocate(analysis_url, .after = analysis_type) %>%
  group_by(
    country,
    analysis_period_start,
    analysis_period_end,
    analysis_type
  ) %>%
  filter(
    date_of_analysis == max(date_of_analysis)
  ) %>%
  ungroup()

# get differences between current values and the previous current value
df_cur_delta <- df_ipc %>%
  filter(
    analysis_type == "current"
  ) %>%
  group_by(
    iso3,
    country
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
  slice(-1) %>% # drop the first observations where change is NA
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
    iso3,
    country,
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
  slice(-1) %>% # drop the first observations where change is NA
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
  ) %>%
  select(
    # ordering for consisent saving out
    iso3,
    country,
    anl_id,
    title,
    condition,
    analysis_type,
    analysis_url,
    date_of_analysis,
    analysis_period_start,
    analysis_period_end,
    potential_incomparability,
    population,
    phase_1_num,
    phase_1_pct,
    phase_2_num,
    phase_2_pct,
    phase_3_num,
    phase_3_pct,
    phase_3pl_num,
    phase_3pl_pct,
    phase_4_num,
    phase_4_pct,
    phase_4pl_num,
    phase_4pl_pct,
    phase_5_num,
    phase_5_pct,
    phase_3_pct_delta,
    phase_3pl_pct_delta,
    phase_4_pct_delta,
    phase_4pl_pct_delta,
    phase_5_pct_delta
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
  mutate(
    phase_incr = case_when(
      phase_5_pct_delta > 0 ~ "phase 5",
      phase_4pl_pct_delta > 0 ~ "phase 4+",
      phase_3pl_pct_delta > 0 ~ "phase 3+"
    ),
    phase_incr_pct = case_when(
      phase_5_pct_delta > 0 ~ phase_5_pct_delta,
      phase_4pl_pct_delta > 0 ~ phase_4pl_pct_delta,
      phase_3pl_pct_delta > 0 ~ phase_3pl_pct_delta
    )
  ) %>%
  group_by(
    iso3,
    country,
    latest_flag = date_of_analysis
  ) %>%
  mutate(
    start_date = min(analysis_period_start),
    end_date = max(analysis_period_end)
  ) %>%
  filter(
    !is.na(phase_incr) | (potential_incomparability & all(is.na(phase_incr)))
  ) %>%
  summarize(
    start_date = unique(start_date),
    end_date = unique(end_date),
    message = ifelse(
      any(!is.na(phase_incr)),
      paste(
        c(
          "Increases in food insecure populations are estimated: ",
          paste(
            scales::percent(phase_incr_pct, accuracy = 1),
            "rise in",
            phase_incr,
            "populations in the",
            str_replace(unique(analysis_type), "_", " "),
            "analysis",
            collapse = ", "
          ),
          ".",
          ifelse(
            potential_incomparability,
            "\n\nThe estimated increase could be due due to shifting geographic focus of the analysis, check the IPC/CH for details.",
            ""
          )
        ),
        collapse = ""
      ),
      paste(
        "<div style=\"font-size:10px;padding: 6px 10px 6px 10px;background-color:#CCCCCC;color:#FFFFFF\"><p style=\"margin:0;\">",
        "The most recent IPC estimates may have a different geographic focus than the",
        "previous estimates, so even though no increase in population was detected",
        "an alert has been generated. Refer to the IPC/CH for more details.",
        "</p></div>"
      )
    ),
    url = unique(analysis_url)[1],
    .groups = "drop"
  ) %>%
  mutate(
    flag_type = "food_insecurity",
    flag_source = "ipc",
    .before = "start_date"
  ) %>%
  relocate(
    latest_flag,
    .after = end_date
  )

##########################################
#### COMPARE WITH PREVIOUS FLAGS FILE ####
##########################################

# load previous flags
df_ipc_flags_prev <- read_gs_file("flags_ipc") %>%
  mutate(
    email = FALSE
  )

# get the difference between the two
df_ipc_flags_new <- anti_join(
  df_ipc_flags,
  df_ipc_flags_prev,
  by = c("iso3", "start_date", "end_date")
) %>%
  mutate(
    email = end_date - Sys.Date() > -90
  )

######################
#### WEB SCRAPING ####
######################

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
      prompt = "Please summarize the current food insecurity situation in 4 sentences based on the following description -->",
      info = txt[1]
    )
    recs <- ai_summarizer(
      prompt = "In 4 sentences, summarize the key recommendations and actions describes below -->",
      info = txt[2]
    )

    # ensure that we are only using those that are not blank
    # so make sure to check when parts of it are not available
    if (is.na(sit_rep) & is.na(recs)) {
      NA
    } else if (is.na(sit_rep)) {
      paste(
        "\n\n<b>Recommendations summary:</b></n> ",
        recs
      )
    } else if (is.na(recs)) {
      paste(
        "<b> Situation summary: </b> ",
        sit_rep
      )
    } else {
      paste(
        "<b> Situation summary: </b> ",
        sit_rep,
        "\n\n<b>Recommendations summary:</b></n> ",
        recs
      )
    }
  } else {
    NA
  }
}

df_ipc_flags_new$summary_experimental <- map_chr(
  df_ipc_flags_new$url,
  ipc_scraper,
  .progress = TRUE
)

# get the summaries and add to the flag files

df_ipc_flags_summary <- bind_rows(
  df_ipc_flags_prev,
  df_ipc_flags_new
) %>%
  select(
    iso3,
    start_date,
    end_date,
    email,
    summary_experimental
  )

df_ipc_flags <- left_join(
  df_ipc_flags,
  df_ipc_flags_summary,
  by = c("iso3", "start_date", "end_date")
)

###########################################
#### UPDATE WRANGLED FILE FOR PLOTTING ####
###########################################

# only keeping current data and latest projections
df_ipc_wrangled_final <- df_ipc_wrangled %>%
  group_by(
    iso3
  ) %>%
  filter( # keep only latest projections
    analysis_type == "current" | date_of_analysis == max(date_of_analysis)
  )

########################
#### SAVE IPC  DATA ####
########################

update_gs_file(
  df = df_ipc_raw,
  name = "raw_ipc"
)

update_gs_file(
  df = df_ipc_wrangled_final,
  name = "wrangled_ipc"
)

update_gs_file(
  df = df_ipc_flags,
  name = "flags_ipc"
)
