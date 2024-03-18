## March 18, 2024

### System

- Data storage solution ported from Google Cloud Platform to Microsoft Azure
Data Storage containers.

## March 15, 2024

### Indicator: IPC

- Adjusted methods to remove duplicate flags from continually being generated.
- Switched to the updated {ripc} package to remove the temporary `ipc_get.R`
fix in this repository.

## January 30, 2024

### Overall

- Changed repository name to `hdx-signals` to begin work to shift system for
public release.

## January 15, 2024

### System

- Added utilities module `update_recipients.R` to streamline adding and removing
recipients from the mailing lists.

## January 4, 2024

### Indicator: IPC

- IPC API disruption across the new year received temporary hotfix that has been
removed once the API was repaired.

## December 18, 2023

### System

- Storage solution for all data moved to Google Cloud Storage `global-monitoring`
bucket. `google_drive.R` deprecated, and all functionality moved to `cloud_storage.R`.
Some remnants of Google Sheets remain to update data that
CERF relies on, but long term the `google_drive.R` module will be removed.
- GitHub Actions file searching to update source indicators changed from using
`find -exec` to globbing, since `find` doesn't explicitly fail. See here:
https://apple.stackexchange.com/questions/49042/how-do-i-make-find-fail-if-exec-fails

### Documentation

- All documentation adjusted to reflect shift from Google Drive/Sheets to 
Google Cloud Storage.

### Indicator: IPC

- Link scraping fixed to not search Cadre Harmonisé links since they do not have
the consistent structure of IPC outputs.

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
