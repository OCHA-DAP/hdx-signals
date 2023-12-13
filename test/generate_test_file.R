# The test dataset is used to test changes to the email system, such as shifts
# in formatting of email.Rmd. To do this, we have a test dataset in the same
# format as flags_total
box::use(dplyr)
box::use(cs = ../src/utils/cloud_storage)

#############################
#### READ IN FLAGS TOTAL ####
#############################

flags_total <- cs$read_gcs_file(
  name = "output/flags_total.parquet"
)

##########################
#### CREATE TEST DATA ####
##########################

# flag specific countries for emailing
flags_total |>
  dplyr$mutate(
    email = dplyr$case_when(
      iso3 == "CAF" & start_date == "2023-04-01" & flag_type == "food_insecurity" ~ TRUE, # CAR FS with warning about incomparability
      iso3 == "MLI" & start_date == "2023-06-01" & flag_type == "food_insecurity" ~ TRUE, # MLI FS with populations in phase 5
      iso3 == "PHL" & start_date == "2023-03-21" & flag_type == "displacement" ~ TRUE, # PHL displacement
      iso3 == "SSD" & start_date == "2022-11-28" & flag_type == "displacement" ~ TRUE, # SSD displacement
      iso3 == "ETH" & start_date == "2022-09-17" & flag_type == "cholera" ~ TRUE, # ETH cholera
      TRUE ~ FALSE # ensure all other emails False
    )
  ) |>
  (
    \(df) { # only temporarily set not as test run to ensure that data is saved out
      orig <- Sys.getenv("GMAS_TEST_RUN", unset = NA)
      Sys.setenv(GMAS_TEST_RUN = FALSE)
      cs$update_gcs_file(
        df = df,
        name = "input/flags_test.parquet"
      )
      # reset
      if (is.na(orig)) {
        Sys.unsetenv("GMAS_TEST_RUN")
      } else {
        Sys.setenv(GMAS_TEST_RUN = orig)
      }
    }
  )()

