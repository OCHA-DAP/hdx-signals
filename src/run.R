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

today <- Sys.Date()

#########################
#### LOAD INDICATORS ####
#########################

# loads all indicators

walk(
  .x = list.files(inds_dir),
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
  pattern = paste0(today, "(.*)_flags.csv")
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
      paste0(today, "_flags_total.csv")
    )
  )
