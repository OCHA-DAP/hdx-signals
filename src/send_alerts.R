box::use(dplyr)
box::use(purrr)
box::use(tidyr)

# local modules
box::use(gs = ./utils/google_sheets)
box::use(./utils/email)

# the scripts to update the source datasets are called in a bash script
# in the GitHub actions workflow. If you need to manually run an update
# then make sure that you run the necessary scripts and update the
# flags_... Google Sheets files before loading them in here and running the
# emails.

#############################
#### CREATE GLOBAL FLAGS ####
#############################

# creates global flags files

ind_flags <- c("flags_ipc", "flags_idmc", "flags_cholera")

flags_total <- purrr$map(
  .x = ind_flags,
  .f = \(x) gs$read_gs_file(name = x, col_types = "ccccDDccclc")
) |>
  purrr$list_rbind() |>
  dplyr$arrange(
    iso3,
    country,
    start_date,
    end_date
  )

# create long format dataset for filtration on the dashboard

flags_total_daily <- flags_total |>
  dplyr$filter(
    flag_type != "cholera"
  ) |>
  dplyr$rowwise() |>
  dplyr$mutate(
    date = list(seq(start_date, end_date, by = "day")),
    .before = start_date
  ) |>
  tidyr$unnest(
    cols = date
  )

######################
#### UPDATE DRIVE ####
######################

gs$update_gs_file(
  df = flags_total,
  name = "flags_total"
)

gs$update_gs_file(
  df = flags_total_daily,
  name = "flags_total_daily"
)

########################
#### GENERATE EMAIL ####
########################

# load in previously sent emails
df_emailed <- gs$read_gs_file(
  name = "flags_emailed"
)

# ensure that emails are not sent again within one month of being sent
flags_email <- dplyr$filter(flags_total, email) |>
  dplyr$anti_join(
    dplyr$filter(
      df_emailed, # get emails sent in past 30 days
      Sys.Date() - email_date <= 30
    ),
    by = c("iso3", "flag_type", "flag_source")
  )

purrr$pwalk(
  .l = flags_email |>
    dplyr$distinct(
      flag_type,
      flag_source
    ),
  .f = \(flag_type, flag_source) email$send_email(flag_type, flag_source, flags_email, FALSE)
)

##############################
#### UPDATE FLAGS EMAILED ####
##############################

df_emailed_update <- dplyr$bind_rows(
  df_emailed,
  flags_email |>
    dplyr$select(
      -email
    ) |>
    dplyr$mutate(
      email_date = Sys.Date()
    )
)

gs$update_gs_file(
  df = df_emailed_update,
  name = "flags_emailed"
)
