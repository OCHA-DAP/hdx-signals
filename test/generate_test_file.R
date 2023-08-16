# The test dataset is used to test changes to the email system, such as shifts
# in formatting of email.Rmd. To do this, we have a test dataset in the same
# format as flags_total
library(tidyverse)

source(
  file.path(
    "src",
    "utils",
    "google_sheets.R"
  )
)

#############################
#### READ IN FLAGS TOTAL ####
#############################

flags_total <- read_gs_file(
  name = "flags_total"
)

##########################
#### CREATE TEST DATA ####
##########################

# flag specific countries for emailing
flags_total %>%
  mutate(
    email = case_when(
      iso3 == "CAF" & start_date == "2023-04-01" & flag_type == "food_insecurity" ~ TRUE, # CAR FS with warning about incomparability
      iso3 == "MLI" & start_date == "2023-06-01" & flag_type == "food_insecurity" ~ TRUE, # MLI FS with populations in phase 5
      iso3 == "PHL" & start_date == "2023-03-21" & flag_type == "displacement" ~ TRUE, # PHL displacement
      iso3 == "SSD" & start_date == "2022-11-28" & flag_type == "displacement" ~ TRUE, # SSD displacement
      iso3 == "ETH" & start_date == "2022-09-17" & flag_type == "cholera" ~ TRUE, # ETH cholera
      TRUE ~ FALSE # ensure all other emails False
    )
  ) %>%
  update_gs_file(
    name = "flags_test"
  )
