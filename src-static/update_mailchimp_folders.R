box::use(httr2)
box::use(purrr)
box::use(dplyr)

box::use(../src/email/mailchimp/base_api)
box::use(cs = ../src/utils/cloud_storage)

base_api$mc_api(lists_api = FALSE) |>
  httr2$req_url_path_append(
    "template-folders"
  ) |>
  httr2$req_perform() |>
  httr2$resp_body_json() |>
  purrr$pluck("folders") |>
  dplyr$bind_rows() |>
  dplyr$distinct(
    name, id
  )
