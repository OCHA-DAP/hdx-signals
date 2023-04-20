## Detect new alerts from global monitoring and email notifications
## Nov 2022

# load libraries
library(tidyverse)
library(googledrive)
library(mailR)

# get Google token
drive_auth(cache = ".secrets")

# get alerts data file ID
alert_file_id <- googledrive::drive_find(pattern = "global-monitoring-mock-alerts.csv")$id ## CHANGE FILE NAME WHEN AVAILABLE

# read in data
alerts_dt <- googledrive::drive_read_string(file = alert_file_id, encoding = "UTF-8") %>%
  read.csv(text = .) %>%
  # format data
  mutate(timestamp_utc = as.POSIXct(timestamp_utc, tz = "UTC"),
         timestamp_et = format(timestamp_utc, tz = "America/New_York", usetz = TRUE, "%Y-%m-%d")) %>%
  select(-timestamp_utc) %>%
  arrange(., desc(timestamp_et))

# prepare body message
url <- "http://www.humdata.shinyapps.io/GlobalMonitoring"
recipients <- c("")

# send message
send.mail(from = "Centre for Humanitarian Data Monitoring <chd.predictive.analytics@gmail.com>",
          to = c("Recipient <user@recipient.com>"),
          # to = c("Recipient 1 <user1@recipient.com>", "Recipient 2 <user@recipient.com>"),
          #  cc = c("CC Recipient <cc.user@recipient.com>"),
          #  bcc = c("BCC Recipient <bcc.user@recipient.com>"),
          # replyTo = c("CHD team <reply.to.user@recipient.com>"),
          subject = paste0("! Monitoring Alerts - ", Sys.Date()),
          body = paste0("New alerts available. Please see report in attachment for detail or visit ", url),
          smtp = list(host.name = "smtp.gmail.com", port = 25,
                      user.name = "chd.predictive.analytics",
                      passwd = Sys.getenv("GOOGLE_APP_PWD"), ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          html = TRUE)
#attach.files = c("kable-extra.png"))
