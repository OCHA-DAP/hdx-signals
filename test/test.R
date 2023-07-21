library(tidyverse)
library(blastula)

source(
  file.path(
    "src",
    "utils",
    "google_sheets.R"
  )
)

#########################
#### LOAD TEST FLAGS ####
#########################

flags_total <- read_gs_file("flags_test")

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
df_recipients <- read_gs_file("email_recipients") %>%
  filter(
    test
  )

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
          gsub(" 0", " ", format(Sys.Date(), " - %d %B %Y")),
          " - TEST ALERT"
        ),
        credentials = email_creds
      )
  }
)
