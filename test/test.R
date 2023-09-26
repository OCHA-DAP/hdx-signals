box::use(dplyr)
box::use(purrr)

# local modules
box::use(gs = ../src/utils/google_sheets)
box::use(../src/utils/email)

#########################
#### LOAD TEST FLAGS ####
#########################

flags_total <- gs$read_gs_file("flags_test")

########################
#### GENERATE EMAIL ####
########################

flags_email <- dplyr$filter(flags_total, email)

purrr$pwalk(
  .l = flags_email |>
    dplyr$distinct(
      flag_type,
      flag_source
    ),
  .f = \(flag_type, flag_source) email$send_email(flag_type, flag_source, flags_email, TRUE)
)
