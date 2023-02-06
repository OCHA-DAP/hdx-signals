library(tidyverse)

inds_dir <- file.path(
  "src",
  "indicators"
)

output_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "exploration",
  "glb",
  "global_monitoring",
  "outputs"
)

#########################
#### LOAD INDICATORS ####
#########################

# runs all indicator scripts

walk(
  .x = list.files(
    inds_dir,
    recursive = TRUE,
    pattern = "^run.R$"
  ),
  .f = ~source(
    file.path(inds_dir, .x),
    local = TRUE,
    verbose = FALSE
  )
)

#############################
#### CREATE GLOBAL FLAGS ####
#############################

# creates global flags files

flags_total <- list.files(
  output_dir,
  recursive = TRUE,
  pattern = "^flags.csv$"
) %>%
  map_dfr(
    ~read_csv(
      file.path(
        output_dir,
        .x
      )
    )
  ) %>%
  arrange(
    iso3,
    country,
    start_date,
    end_date
  )

flags_total %>%
  write_csv(
    file.path(
      output_dir,
     "flags_total.csv"
    ),
    na = ""
  )

# create long format dataset for filtration on the dashboard

flags_total %>%
  rowwise() %>%
  mutate(
    date = list(seq(start_date, end_date, by = "day")),
    .before = start_date
  ) %>%
  unnest(
    cols = date
  ) %>%
  write_csv(
    file.path(
      output_dir,
      "flags_total_daily.csv"
    ),
    na = ""
  )
