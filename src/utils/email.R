library(blastula)

email_creds <- creds_envvar(
  user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
  pass_envvar = "CHD_DS_EMAIL_PASSWORD",
  host = Sys.getenv("CHD_DS_HOST"),
  port = 465,
  use_ssl = TRUE
)

smtp_send(
  email = compose_email("test"),
  to = "seth.caldwell@un.org",
  from = "data.science@humdata.org",
  credentials = email_creds
)
