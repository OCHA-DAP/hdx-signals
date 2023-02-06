library(idmc)
library(tidyverse)
library(lubridate)
library(openai)

output_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "exploration",
  "glb",
  "global_monitoring",
  "outputs"
)

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
    country,
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
    country,
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
  ungroup()

# get total displacement within each of the flags
# difficult for any flag because start date depends on what kind of flags have
# generated the flags within it

df_flags_start <- df_idmc_flags %>%
  filter(
    flag_name != "flag_idmc_any"
  ) %>%
  mutate(
    start_date = case_when(
      str_detect(flag_name, "yearly") ~ start_date - days(364),
      str_detect(flag_name, "quarterly") ~ start_date - days(89),
      str_detect(flag_name, "monthly") ~ start_date - days(29),
      str_detect(flag_name, "weekly") ~ start_date - days(6),
      TRUE ~ start_date
    )
  )

# for any flags, join up to the other data set
# and make the start date the minimum of the start date of any
# other flag in that time frame
df_flags_start_any <- df_idmc_flags %>%
  filter(
    flag_name == "flag_idmc_any"
  ) %>%
  full_join(
    rename_with(
      df_flags_start,
      .fn = ~ paste0(.x, 2),
      .cols = ends_with("date")
    ) %>%
      select(
        country, ends_with("date2")
      ),
    by = "country"
  ) %>%
  group_by(iso3, country, flag_name, uuid) %>%
  filter(
    end_date2 >= start_date
  ) %>%
  summarize(
    start_date = min(start_date2),
    end_date = unique(end_date),
    .groups = "drop"
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
      country,
      date,
      displacement_daily
    ),
    by = "country"
  ) %>%
  filter(
    date >= start_date,
    date <= end_date
  ) %>%
  group_by(
    iso3, country, flag_name, uuid, start_date, end_date
  ) %>%
  summarize(
    total_displacement = sum(displacement_daily),
    message = paste0(
      scales::number(total_displacement, big.mark = ","),
      " people have been displaced between ",
      format(min(start_date), format = "%B %d %Y"),
      " and ",
      format(max(end_date), format = "%B %d %Y"),
      "."
    ),
    .groups = "drop"
  )

# now join back to the original flags dataset with the full displacement data
df_idmc_flags_final <- df_idmc_flags %>%
  left_join(
    select(df_displacement, -ends_with("date")),
    by = c("iso3", "country", "flag_name", "uuid")
  ) %>%
  mutate(
    flag_type = "displacement",
    flag_source = "idmc",
    .before = flag_name
  ) %>%
  filter(
    flag_name == "flag_idmc_any"
  )

#######################
#### GPT EXPLAINER ####
#######################

df_explain <- df_displacement %>%
  filter(
    flag_name == "flag_idmc_any"
  ) %>%
  select(
    country, start_date, end_date, total_displacement
  )

ai_summary <- pmap_chr(
  df_explain,
  \(country, start_date, end_date, total_displacement) {
    # get all reports of displacement
    reports <- df_idmc_raw %>%
      filter(
        country == !!country,
        displacement_end_date >= !!start_date,
        displacement_start_date <= !!end_date
      ) %>%
      pull(
        event_info
      )

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
      "Only use information from the the below reports:",
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

df_idmc_flags_final$summary_experimental <- str_remove_all(ai_summary, "\\\n")

# filter final data

df_idmc_flags_final <- df_idmc_flags_final %>%
  select(
    iso3,
    country,
    flag_type,
    flag_source,
    start_date,
    end_date,
    message,
    summary_experimental
  )

#########################
#### SAVING OUT DATA ####
#########################

write_csv(
  x = df_idmc_raw,
  file = file.path(
    output_dir,
    "idmc",
    "raw.csv"
  ),
  na = ""
)

write_csv(
  x = df_idmc,
  file = file.path(
    output_dir,
    "idmc",
    "wrangled.csv"
  ),
  na = ""
)

write_csv(
  x = df_idmc_flags_final,
  file = file.path(
    output_dir,
    "idmc",
    "flags.csv"
  ),
  na = ""
)
