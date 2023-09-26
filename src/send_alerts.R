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

flags_email <- dplyr$filter(flags_total, email)

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

df_emailed <- gs$read_gs_file(
  name = "flags_emailed"
) |>
  dplyr$bind_rows(
    flags_email |>
      dplyr$select(
        -email
      ) |>
      dplyr$mutate(
        email_date = Sys.Date()
      )
  )

gs$update_gs_file(
  df = df_emailed,
  name = "flags_emailed"
)
