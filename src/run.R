library(tidyverse)
library(knitr)

source(
  file.path(
    "src",
    "utils",
    "googledrive.R"
  )
)

inds_dir <- file.path(
  "src",
  "indicators"
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

ind_flags <- c("flags_ipc", "flags_idmc")

flags_total <- map(
  .x = ind_flags,
  .f = function(x) {
      drive_file <- get_drive_file(x)
      drive_download(file = drive_file, path = f <- tempfile(fileext = ".csv"))
      read_csv(f)
    }
  ) %>%
  list_rbind() %>%
  arrange(
    iso3,
    country,
    start_date,
    end_date
  )

# create long format dataset for filtration on the dashboard

flags_total_daily <- flags_total %>%
  rowwise() %>%
  mutate(
    date = list(seq(start_date, end_date, by = "day")),
    .before = start_date
  ) %>%
  unnest(
    cols = date
  )

######################
#### UPDATE DRIVE ####
######################

update_drive_file(
  df = flags_total,
  local_path = tempfile(fileext = ".csv"),
  drive_file = get_drive_file("flags_total.csv")
)

update_drive_file(
  df = flags_total_daily,
  local_path = tempfile(fileext = ".csv"),
  drive_file = get_drive_file("flags_total_daily")
)

########################
#### GENERATE EMAIL ####
########################

flags_email <- filter(flags_total, email)

# plotting functions
source(
  file.path(
    "src",
    "plots",
    "plot.R"
  )
)

if (nrow(flags_email) > 0) {
  knit(
    input = file.path(
      "src",
      "email",
      "email.Rmd"
    )
  )
}

