box::use(./audience)
box::use(cs = ../../utils/cloud_storage)

# simply directly backup the Mailchimp audience as a JSON file to Azure container

member_list <- audience$mc_members()

cs$update_az_file(
  df = member_list,
  name = "backup/mc_audience.json"
)
