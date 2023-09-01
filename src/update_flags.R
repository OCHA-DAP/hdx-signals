box::use(dplyr)
box::use(purrr)
box::use(tidyr)

# local modules
box::use(gs = utils/google_sheets)

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
  ) %>%
  dplyr$rowwise() %>%
  dplyr$mutate(
    date = list(seq(start_date, end_date, by = "day")),
    .before = start_date
  ) %>%
  tidyr$unnest(
    cols = date
  )

######################
#### UPDATE DRIVE ####
######################

update_gs_file(
  df = flags_total,
  name = "flags_total"
)

update_gs_file(
  df = flags_total_daily,
  name = "flags_total_daily"
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

# Load in e-mail credentials
email_creds <- creds_envvar(
  user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
  pass_envvar = "CHD_DS_EMAIL_PASSWORD",
  host = Sys.getenv("CHD_DS_HOST"),
  port = Sys.getenv("CHD_DS_PORT"),
  use_ssl = TRUE
)

# load in recipients
df_recipients <- read_gs_file("email_recipients")

pwalk(
  .l = flags_email %>%
    distinct(
      flag_type,
      flag_source
    ),
  .f = \(
    flag_type,
    flag_source
  ) {
    render_email(
      input = file.path(
        "src",
        "email",
        "email.Rmd"
      )
    ) %>%
      smtp_send(
        to = filter(df_recipients, to)$email,
        bcc = filter(df_recipients, !to)$email,
        from = "data.science@humdata.org",
        subject = paste0(
          "Monitoring Alert: ",
          str_replace_all(str_to_title(flag_type), "_", " "),
          gsub(" 0", " ", format(Sys.Date(), " - %d %B %Y"))
        ),
        credentials = email_creds
      )
  }
)

##############################
#### UPDATE FLAGS EMAILED ####
##############################

df_emailed <- read_gs_file(
  name = "flags_emailed"
) %>%
  bind_rows(
    flags_email %>%
      select(
        -email
      ) %>%
      mutate(
        email_date = Sys.Date()
      )
  )

update_gs_file(
  df = df_emailed,
  name = "flags_emailed"
)
