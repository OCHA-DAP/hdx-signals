# The test dataset is used to test changes to the email system, such as shifts
# in formatting of email.Rmd. To do this, we have a test dataset in the same
# format as flags_total
library(tidyverse)

source(
  file.path(
    "src",
    "utils",
    "googledrive.R"
  )
)

#############################
#### READ IN FLAGS TOTAL ####
#############################

drive_download(
  file = get_drive_file("flags_total.csv"),
  path = f <- tempfile(fileext = ".csv")
)

flags_total <- read_csv(f)

##########################
#### CREATE TEST DATA ####
##########################
f <- tempfile(fileext = ".csv")

# flag specific countries for emailing
flags_total %>%
  mutate(
    email = case_when(
      iso3 == "CAF" & start_date == "2023-04-01" & flag_type == "food_insecurity" ~ TRUE, # CAR FS with warning about incomparability
      iso3 == "MLI" & start_date == "2023-06-01" & flag_type == "food_insecurity" ~ TRUE, # MLI FS with populations in phase 5
      iso3 == "NGA" & start_date == "2023-03-15" & flag_type == "displacement" ~ TRUE, # NGA displacement
      iso3 == "SSD" & start_date == "2022-11-28" & flag_type == "displacement" ~ TRUE, # SSD displacement
      TRUE ~ FALSE # ensure all other emails False
    )
  ) %>%
  write_csv(
    file = f
  )

drive_update(
  file = get_drive_file("flags_test"),
  media = f
)
