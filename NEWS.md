## December 13, 2023

### System

- Storage solution for all data moved to Google Cloud Storage `global-monitoring`
bucket. `google_drive.R` changed to `cloud_storage.R`, and all usage of the functionality
moved to the new setup. Some remnants of Google Sheets remain to update data that
CERF relies on, but long term this will be removed from the system entirely.

### Documentation

- All documentation adjusted to reflect shift from Google Drive/Sheets to 
Google Cloud Storage 

## December 11, 2023

### Indicator: cholera

- Adjusted start date parsing adjusted to address change in date format from
`%d-%b-%y` to `%m/%d/%Y` in the WHO AFRO bulletins.

## October 3, 2023

### Email alerts

- Added system check to `send_alerts.R` so that emails for the same country and
shock are not sent again within 30 days.

### Documentation

- Implemented NEWS.md to track changes to the repository.

### Indicator: IDMC

- Removed yearly displacement flagging since the timescale was too long for
meaningful alerts.
- Removed flagging for first displacement in a year and just left at 180 days.
- Added country names to email titles.
- Changed message to report displacement since start date without listing the end
date to ensure that all relevant recent displacement is included in the message.

### Utilities: `flagging.R`

- Set minimums for `calculate_flags()` to be `-Inf` rather than `0` to be flexible for
datasets with negatives.
- Implemented `first_since_minimums` argument for `calculate_flags()` to only flag
if the values are above the minimum.
- Calculate `data_sum` column in `generate_alerts()` to generate sum of displacement
since start of alert to 60 days since the end of the alert.
