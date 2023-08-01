library(tidyverse)
library(blastula)

source(
  file.path(
    "src",
    "utils",
    "google_sheets.R"
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

ind_flags <- c("flags_ipc", "flags_idmc", "flags_cholera")

flags_total <- map(
  .x = ind_flags,
  .f = read_gs_file,
  col_types = "ccccDDccclc"
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
  filter(
    flag_type != "cholera"
  ) %>%
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
