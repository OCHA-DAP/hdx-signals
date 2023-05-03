library(tidyverse)
library(blastula)

source(
  file.path(
    "src",
    "utils",
    "googledrive.R"
  )
)

#########################
#### LOAD TEST FLAGS ####
#########################

drive_download(
  file = get_drive_file("flags_test"),
  path = f <- tempfile(fileext = ".csv")
)

flags_total <- read_csv(f)

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
drive_recipients <- get_drive_file("email_recipients")
drive_download(drive_recipients, f <- tempfile(fileext = ".csv"))
df_recipients <- read_csv(f) %>%
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
        subject = paste(
          "GMA",
          str_replace_all(str_to_title(flag_type), "_", " "),
          str_to_upper(flag_source),
          format(Sys.Date(), "%Y %B %d"),
          "TEST ALERT",
          sep = " - "
        ),
        credentials = email_creds
      )
  }
)
