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

list.files(
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
  ) %>%
  write_csv(
    file.path(
      output_dir,
     "flags_total.csv"
    )
  )
