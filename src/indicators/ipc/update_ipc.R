# external packages
box::use(ripc)
box::use(dplyr)
box::use(countrycode)
box::use(stringr)
box::use(scales)
box::use(rvest)
box::use(tidyr)
box::use(purrr)

# internal utilities
# first set the root search path for utilities
box::use(cs = ../../utils/cloud_storage)
box::use(gd = ../../utils/google_drive)
box::use(../../utils/ai_summarizer[ai_summarizer])
box::use(../../utils/get_country_names[get_country_names])
box::use(ipc_fix = ../../utils/ipc_get)


#############
#### IPC ####
#############

# get into country level form and correct the names
df_ipc_raw <- ipc_fix$ipc_get_population()$country |>
  dplyr$filter(
    condition == "A" # only use acute for now, chronic doesn't have date info
  )

# separately run countrycode to avoid warning for no match for LAC
df_ipc_raw[df_ipc_raw$country == "LAC", "iso3"] <- "LAC"
df_ipc_raw[df_ipc_raw$country != "LAC", "iso3"] <- countrycode$countrycode(
  df_ipc_raw$country[df_ipc_raw$country != "LAC"],
  origin = "iso2c",
  destination = "iso3c"
)

df_ipc_raw <- get_country_names(df_ipc_raw)

# now create the wrangled file
df_ipc <- df_ipc_raw |>
  dplyr$rename_with(
    .fn = ~stringr$str_replace(.x, "phase", "phase_")
  ) |>
  dplyr$rename(
    date_of_analysis = analysis_date,
    analysis_type = period,
    population = estimated_population
  ) |>
  dplyr$mutate(
    phase_4pl_num = phase_4_num + phase_5_num,
    phase_4pl_pct = phase_4_pct + phase_5_pct
  ) |>
  dplyr$left_join(
    ripc$ipc_get_analyses() |>
      dplyr$transmute(
        analysis_id,
        analysis_url = stringr$str_replace(link, "http://www-test.fao.org/ipcinfo-portal", "https://www.ipcinfo.org")
      ),
    by = "analysis_id"
  ) |>
  dplyr$relocate(analysis_url, .after = analysis_type) |>
  dplyr$group_by(
    country,
    analysis_period_start,
    analysis_period_end,
    analysis_type
  ) |>
  dplyr$filter(
    date_of_analysis == max(date_of_analysis)
  ) |>
  dplyr$ungroup()

# get differences between current values and the previous current value
df_cur_delta <- df_ipc |>
  dplyr$filter(
    analysis_type == "current"
  ) |>
  dplyr$group_by(
    iso3,
    country
  ) |>
  dplyr$arrange(
    date_of_analysis,
    .by_group = TRUE
  ) |>
  dplyr$mutate(
    dplyr$across(
      dplyr$matches("pct$"),
      ~ .x - dplyr$lag(.x),
      .names = "{col}_delta"
    ),
    title_detail = stringr$str_extract(title, "(?<=[0-9]{4})(.*)$"),
    potential_incomparability = title_detail != dplyr$lag(title_detail)
  ) |>
  dplyr$slice(-1) |> # drop the first observations where change is NA
  dplyr$ungroup() |>
  dplyr$select(
    analysis_id,
    analysis_type,
    dplyr$ends_with("delta"),
    potential_incomparability
  )

# get differences between current and projections (or first to second proj)
df_proj_delta <- df_ipc |>
  dplyr$group_by(
    iso3,
    country,
    date_of_analysis
  ) |>
  dplyr$filter(
    "projected" %in% analysis_type
  ) |>
  dplyr$arrange(
    analysis_type,
    .by_group = TRUE
  ) |>
  dplyr$mutate(
    dplyr$across(
      dplyr$ends_with("pct"),
      ~ .x - dplyr$lag(.x),
      .names = "{col}_delta"
    )
  ) |>
  dplyr$slice(-1) |> # drop the first observations where change is NA
  dplyr$ungroup() |>
  dplyr$select(
    analysis_id,
    analysis_type,
    dplyr$ends_with("delta")
  )

######################
#### WRANGLE DATA ####
######################

# wrangle the data together for a single country-level IPC dataset

df_ipc_wrangled <- dplyr$left_join(
  df_ipc,
  dplyr$bind_rows(df_cur_delta, df_proj_delta),
  by = c("analysis_id", "analysis_type")
) |>
  dplyr$mutate(
    potential_incomparability = tidyr$replace_na(potential_incomparability, FALSE)
  ) |>
  dplyr$select(
    # ordering for consisent saving out
    iso3,
    country,
    analysis_id,
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
df_ipc_flags <- df_ipc_wrangled |>
  dplyr$filter(
    dplyr$if_any(
      .cols = c(phase_3pl_pct_delta, phase_4pl_pct_delta, phase_5_pct_delta),
      .fns = ~ !is.na(.x) & .x > 0
    ) | potential_incomparability
  ) |>
  dplyr$mutate(
    phase_incr = dplyr$case_when(
      phase_5_pct_delta > 0 ~ "phase 5",
      phase_4pl_pct_delta > 0 ~ "phase 4+",
      phase_3pl_pct_delta > 0 ~ "phase 3+"
    ),
    phase_incr_pct = dplyr$case_when(
      phase_5_pct_delta > 0 ~ phase_5_pct_delta,
      phase_4pl_pct_delta > 0 ~ phase_4pl_pct_delta,
      phase_3pl_pct_delta > 0 ~ phase_3pl_pct_delta
    )
  ) |>
  dplyr$group_by(
    iso3,
    country,
    latest_flag = date_of_analysis
  ) |>
  dplyr$mutate(
    start_date = min(analysis_period_start),
    end_date = max(analysis_period_end)
  ) |>
  dplyr$filter(
    !is.na(phase_incr) | (potential_incomparability & all(is.na(phase_incr)))
  ) |>
  dplyr$summarize(
    start_date = unique(start_date),
    end_date = unique(end_date),
    message = ifelse(
      any(!is.na(phase_incr)),
      paste(
        c(
          "Increases in food insecure populations are estimated: ",
          paste(
            scales$percent(phase_incr_pct, accuracy = 1),
            "rise in",
            phase_incr,
            "populations in the",
            stringr$str_replace(unique(analysis_type), "_", " "),
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
  ) |>
  dplyr$mutate(
    flag_type = "food_insecurity",
    flag_source = "ipc",
    .before = "start_date"
  ) |>
  dplyr$relocate(
    latest_flag,
    .after = end_date
  )

##########################################
#### COMPARE WITH PREVIOUS FLAGS FILE ####
##########################################

# load previous flags
df_ipc_flags_prev <- cs$read_gcs_file("output/ipc/flags.parquet") |>
  dplyr$mutate(
    email = FALSE
  )

# get the difference between the two
df_ipc_flags_new <- dplyr$anti_join(
  df_ipc_flags,
  df_ipc_flags_prev,
  by = c("iso3", "start_date", "end_date", "latest_flag")
) |>
  dplyr$group_by(
    iso3, start_date, end_date
  ) |>
  dplyr$mutate(
    email = end_date - Sys.Date() > - 90 & latest_flag == max(latest_flag, -Inf)
  )

######################
#### WEB SCRAPING ####
######################

# scrape the IPC URL for additional information and pass that on to the
# ChatGPT AI model for summarization.
ipc_scraper <- function(url) {
  if (!is.na(url) & url != "http://www.ipcinfo.org/cadre-harmonise") {
    txt <- rvest$read_html(url) |>
      rvest$html_nodes("._undmaptext") |>
      rvest$html_text()
    # extract the situation report and recommendations from the scraped text
    txt <- txt[rank(-nchar(txt)) <= 2]
    txt <- stringr$str_replace_all(txt, "[\r\n\t]+", " ")

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

df_ipc_flags_new$summary_experimental <- purrr$map_chr(
  df_ipc_flags_new$url,
  ipc_scraper,
  .progress = TRUE
)

# get the summaries and add to the flag files

df_ipc_flags_summary <- dplyr$bind_rows(
  df_ipc_flags_prev,
  df_ipc_flags_new
) |>
  dplyr$select(
    iso3,
    start_date,
    end_date,
    latest_flag,
    email,
    summary_experimental
  )

df_ipc_flags <- dplyr$left_join(
  df_ipc_flags,
  df_ipc_flags_summary,
  by = c("iso3", "start_date", "end_date", "latest_flag")
)

###########################################
#### UPDATE WRANGLED FILE FOR PLOTTING ####
###########################################

# only keeping current data and latest projections
df_ipc_wrangled_final <- df_ipc_wrangled |>
  dplyr$group_by(
    iso3
  ) |>
  dplyr$filter( # keep only latest projections
    analysis_type == "current" | date_of_analysis == max(date_of_analysis)
  )

########################
#### SAVE IPC  DATA ####
########################

cs$update_gcs_file(
  df = df_ipc_raw,
  name = "output/ipc/raw.parquet"
)

cs$update_gcs_file(
  df = df_ipc_wrangled_final,
  name = "output/ipc/wrangled.parquet"
)

cs$update_gcs_file(
  df = df_ipc_flags,
  name = "output/ipc/flags.parquet"
)

# TODO: remove all of this google sheets once CERF has shifted their system

gd$update_gs_file(
  df = df_ipc_wrangled_final,
  name = "wrangled_ipc"
)

message("Updated IDMC!")
