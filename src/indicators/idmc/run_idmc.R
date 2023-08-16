library(idmc)
library(tidyverse)
library(lubridate)
library(openai)

source(
  file.path(
    "src",
    "utils",
    "get_country_names.R"
  )
)

# authorize and prep
source(
  file.path(
    "src",
    "utils",
    "google_sheets.R"
  )
)

##############################
#### DRIVE AND LOCAL DATA ####
##############################

# country links on the IDMC page
df_links <- read_gs_file("idmc_country_links")

##############
#### IDMC ####
##############

df_idmc_raw <- idmc_get_data() %>%
  filter(
    displacement_type == "Conflict"
  )

df_idmc <- df_idmc_raw %>%
  idmc_transform_daily() %>%
  idmc_rolling_sum() %>%
  idmc_flagging()

# get the long flags data frame

df_idmc_flags <- df_idmc %>%
  select(
    -flag_total
  ) %>%
  rename_with(
    .fn = ~gsub("(flag_)(.*)", "\\1idmc_\\2", .x),
    .cols = starts_with("flag")
  ) %>%
  pivot_longer(
    cols = starts_with("flag"),
    names_to = "flag_name",
    values_to = "flag"
  ) %>%
  group_by(
    iso3,
    flag_name
  ) %>%
  filter(
    flag
  ) %>%
  mutate(
    cs_temp_ = cumsum(date - lag(date, default = min(date)) != lubridate::days(1))
  ) %>%
  group_by(
    iso3,
    flag_name,
    cs_temp_
  ) %>%
  summarize(
    start_date = min(date),
    end_date = max(date),
    .groups = "drop_last"
  ) %>%
  select(
    -cs_temp_
  ) %>%
  mutate(
    uuid = row_number()
  ) %>%
  ungroup() %>%
  left_join(
    df_links,
    by = "iso3"
  ) %>%
  mutate(
    url = paste0("https://www.internal-displacement.org/countries/", country_link)
  ) %>%
  select(
    -country_link
  )

# get total displacement within each of the flags
# difficult for any flag because start date depends on what kind of flags have
# generated the flags within it

df_flags_start <- df_idmc_flags %>%
  filter(
    flag_name != "flag_idmc_any"
  ) %>%
  mutate(
    flag_start_date = start_date,
    start_date = case_when(
      str_detect(flag_name, "yearly") ~ start_date - days(364),
      str_detect(flag_name, "quarterly") ~ start_date - days(89),
      str_detect(flag_name, "monthly") ~ start_date - days(29),
      str_detect(flag_name, "weekly") ~ start_date - days(6),
      TRUE ~ start_date
    )
  )

# setting flagging priority for the latest flag for a country
# we start with the 1st in a period as priority
# then look at weekly -> yearly in increasing priority so that user sees
# the more acute, short term fluctuations on the main dashboard page
flag_priority <- c("flag_idmc_1st_year", "flag_idmc_1st_6_months",
                   "flag_idmc_1st_3_months", "flag_idmc_global_weekly",
                   "flag_idmc_weekly", "flag_idmc_global_monthly",
                   "flag_idmc_monthly", "flag_idmc_global_quarterly",
                   "flag_idmc_quarterly", "flag_idmc_global_yearly",
                   "flag_idmc_yearly", "flag_idmc_any")

# for any flags, join up to the other data set
# and make the start date the minimum of the start date of any
# other flag in that time frame
df_flags_start_any <- df_idmc_flags %>%
  filter(
    flag_name == "flag_idmc_any"
  ) %>%
  full_join(
      select(
        df_flags_start,
        iso3,
        flag_name2 = flag_name,
        start_date2 = start_date,
        end_date2 = end_date,
        flag_start_date
      ),
    by = "iso3",
    multiple = "all",
    relationship = "many-to-many"
  ) %>%
  group_by(iso3, flag_name, uuid) %>%
  filter(
    end_date2 >= start_date,
    flag_start_date <= end_date
  ) %>%
  mutate(
    start_date = min(start_date2)
  ) %>%
  filter(
    end_date2 == end_date
  ) %>%
  summarize(
    across(
      .cols = c("start_date", "end_date"),
      .fns = unique
    ),
    latest_flag = flag_name2[which.min(match(flag_name2, flag_priority))],
    .groups = "drop",
  )

# now join the two together and get the full displacement across that time
# period to create meaningful messages and information for the end user

df_displacement <- bind_rows(
  df_flags_start,
  df_flags_start_any
) %>%
  full_join(
    select(
      df_idmc,
      iso3,
      date,
      displacement_daily
    ),
    by = "iso3",
    multiple = "all",
    relationship = "many-to-many"
  ) %>%
  filter(
    date >= start_date,
    date <= end_date
  ) %>%
  group_by(
    iso3, flag_name, uuid, start_date, end_date
  ) %>%
  summarize(
    latest_flag = unique(str_remove_all(latest_flag, "flag_idmc_|global_")),
    total_displacement = sum(displacement_daily),
    message = paste0(
      scales::number(total_displacement, big.mark = ","),
      " people were displaced between ",
      gsub("^0", "", format(min(start_date), format = "%d %B %Y")),
      " and ",
      gsub("^0", "", format(max(end_date), format = "%d %B %Y")),
      "."
    ),
    .groups = "drop"
  )

# now join back to the original flags dataset with the full displacement data
# take the full flags and extract the final flags for sharing
df_idmc_flags_full <- df_idmc_flags %>%
  left_join(
    select(df_displacement, -ends_with("date")),
    by = c("iso3", "flag_name", "uuid")
  ) %>%
  mutate(
    flag_type = "displacement",
    flag_source = "idmc",
    .before = flag_name
  ) %>%
  filter(
    flag_name == "flag_idmc_any"
  ) %>%
  select(
    iso3,
    flag_type,
    flag_source,
    start_date,
    end_date,
    uuid,
    latest_flag,
    message,
    url
  )

#####################################
#### COMPARE WITH EXISTING FILES ####
#####################################

df_idmc_flags_final_prev <- read_gs_file("flags_idmc") %>%
  mutate(
    email = FALSE
  )

# all these new data will need new summaries generated by GPT
df_idmc_flags_new <- anti_join(
  df_idmc_flags_full,
  df_idmc_flags_final_prev,
  by = c("iso3", "start_date", "end_date")
)

# we only generate emails for new alerts, however, so need to do more comparison
# with the previous flags

alert_levels <- c("1st_year", "1st_6_months",
                 "1st_3_months", "weekly",
                 "monthly", "quarterly",
                 "yearly")

df_idmc_flags_email <- df_idmc_flags_final_prev %>%
  select(
    iso3,
    start_date_prev = start_date,
    end_date_prev = end_date,
    prev_flag = latest_flag
  ) %>%
  full_join(
    df_idmc_flags_new,
    by = "iso3",
    relationship = "many-to-many"
  ) %>%
  filter(
    !is.na(latest_flag),
    start_date <= start_date_prev,
    end_date >= start_date_prev
  ) %>%
  group_by(
    iso3
  ) %>%
  filter(
    end_date_prev == max(end_date_prev, -Inf)
  ) %>%
  ungroup() %>%
  mutate(
    prev_flag_level = match(prev_flag, alert_levels),
    latest_flag_level = match(latest_flag, alert_levels),
    diff_date = end_date - Sys.Date(),
    email = latest_flag_level < prev_flag_level
  ) %>%
  select(
    iso3,
    uuid,
    email
  )

# some of the new alerts may not have had a previous alert that was ongoing
# so we join up the email column to all new alerts

df_idmc_flags_new_final <- df_idmc_flags_new %>%
  left_join(
    df_idmc_flags_email,
    by = c("iso3", "uuid")
  ) %>%
  mutate(
    email = ifelse(
      Sys.Date() - end_date > 60, # do not email for old events updated
      FALSE,
      replace_na(email, TRUE) # create emails for new alerts
    )
  )

#######################
#### GPT EXPLAINER ####
#######################

df_explain <- df_displacement %>%
  filter(
    flag_name == "flag_idmc_any"
  ) %>%
  semi_join(
    df_idmc_flags_new_final,
    by = c("iso3", "uuid")
  ) %>%
  select(
    iso3, start_date, end_date, total_displacement
  )

ai_summary <- pmap_chr(
  df_explain,
  \(iso3, start_date, end_date, total_displacement) {
    # get all reports of displacement
    df_filtered <- df_idmc_raw %>%
      filter(
        iso3 == !!iso3,
        displacement_end_date >= !!start_date,
        displacement_start_date <= !!end_date
      )
    reports <- df_filtered$event_info
    country <- unique(df_filtered$country)


    # concat into input
    displacement_info <- paste(
      reports,
      collapse = "\n"
    )

    # random sample reports to ensure token length not problem for OpenAI
    while (nchar(displacement_info) > 3800) {
      reports <- reports[-sample(seq_along(reports), size = 1)]
      displacement_info <- paste(reports, collapse = "\n")
    }

    # get AI summarization
    req <- paste(
      "There have been",
      scales::number(total_displacement, big.mark = ","),
      "people displaced by conflict in",
      country,
      "between",
      format(start_date, format = "%B %d %Y"),
      "and",
      format(end_date, format = "%B %d %Y"),
      ". This information comes from a series of small reports.",
      "In a short paragraph, please summarize the main reasons for displacement.",
      "Avoid providing specific numbers or dates, just provide the general",
      "reasons behind the displacement and other key qualitative information.",
      "Only use the below information:",
      displacement_info
    )

    # get AI summarization
    insistent_ai <- insistently(
      \(req) {
        create_completion(
          model = "text-davinci-003",
          prompt = req,
          max_tokens = 100
        )$choices$text
      },
      rate = rate_delay(pause = 3, max_times = 5)
    )

    insistent_ai(req)
  },
  .progress = TRUE
)

df_idmc_flags_new_final$summary_experimental <- str_remove_all(ai_summary, "\\\n")

#################################
#### CREATE FINAL FLAGS FILE ####
#################################

# previous summary messages pulled
df_prev_summary <- inner_join(
  df_idmc_flags_final_prev,
  df_idmc_flags_full,
  by = c("iso3", "start_date", "end_date")
) %>%
  transmute(
    iso3,
    uuid,
    email = FALSE, # no emails for previous data
    summary_experimental
  )

# new summaries for new values
df_new_summary <- df_idmc_flags_new_final %>%
  select(
    iso3,
    uuid,
    email,
    summary_experimental
  )

# create the final flag

df_idmc_flags_final <- df_idmc_flags_full %>%
  left_join(
    bind_rows(df_new_summary, df_prev_summary),
    by = c("iso3", "uuid")
  ) %>%
  select(
    -uuid
  ) %>%
  get_country_names()

########################
#### SAVE IDMC DATA ####
########################

update_gs_file(
  df = get_country_names(df_idmc_raw),
  name = "raw_idmc"
)

update_gs_file(
  df = get_country_names(df_idmc),
  name = "wrangled_idmc"
)

update_gs_file(
  df = df_idmc_flags_final,
  name = "flags_idmc"
)
