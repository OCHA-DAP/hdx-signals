# Changes

## 0.1.14.3 (20 November 2024)

### Runs

- Add `Run user analytics` GitHub Action (run every Monday) to store and compile data for user research/analytics initiative

## 0.1.14.2 (2 October 2024)

### Runs

- Move `acled_conflict` and `idmc_displacement_...` runs to be daily rather than
only on Tuesdays

## 0.1.14.1 (1 October 2024)

### System

- Properly imported `template_data` in `triage_signals.R`
- Fix the admin-0 loading

## 0.1.14.0 (26 September 2024)

### Mapping

- Maps for `acled_conflict` changed to filter out points lying outside the base
administrative boundaries

### idmc_displacement_conflict

- Created custom filtering for IDMC data to ensure time series for countries
are only analyzed after consistent coverage begins, contained in
`input/idmc_dates.parquet`

## 0.1.13.3 (23 September 2024)

### System

- Ensured env variables were properly imported across modules
- Upgraded R version to 4.4.1, upgrading packages to use new `{Matrix}` package
with breaking changes
- Updated all packages in `{renv}` lock file

## 0.1.13.2 (17 September 2024)

### System

- Moved to `{box.linters}` package to allow comprehensive linting of a project
using `{box}` for module management

## 0.1.13.1 (19 August 2024)

### System

- Members call to retrieve the HDX Signals Mailchimp audience fixed to iterate
if the audience is more than 1,000 subscribers, as the API limits returns to
1,000 entries total

- Triage setup fixed so that user input message is not cut off by the character
limit in `readline()`

## 0.1.13.0 (14 August 2024)

### Signals

- Changed the signals detection logic to only look for new signals in the past 60
days. Previously, looked at the past 90 days, but when reviewing a signal from
around 80 days in the past, the team decided this was too old

### System

- Extended buffer for checking if admin boundaries covered geometries to account
for naval encounters when an issue was detected for ACLED in Cameroon

## 0.1.12.0 (6 August 2024)

### System

- Added buffering to administrative boundaries when checking if it covers other
geometries, and simplified the geometries to check for speed

## 0.1.11.1 (2 August 2024)

### System

- Clean up `logger` config by consolidating all setup to `.Rprofile`

## 0.1.11.0 (1 August 2024)

### acled_conflict

- ACLED conflict monitoring scope has expanded to monitor fatalities from all
events, including protests and riots, rather than simply specifically violent
events
- Changed plot and map titles to reflect expanded coverage of all event types,
referring now to `X fatalities` and `Reported events` respectively

### Maps

- Added functionality to reduce the basemap for countries like the USA where certain
geographies (e.g. Alaska) should be dropped if no data is falling within them
- Reduced the cities plotted on the USA basemap as these cluttered the visualizations

### Time series plots

- For `idmc_displacement_{driver}` and `acled_conflict`, time series plots are now
filtered to only show data up until the date that a signal was generated
- Plot data filtering for the same indicators is now done numerically to filter data
every 30 days from the date of the signal, rather than based on day of the month,
to ensure no overlap in rolling sums

## 0.1.10.1 (30 July 2024)

### System

- Turned click tracking back on for new campaigns now that Mailchimp quarantine
issue is likely resolved
- Suppressed geometry warnings in `map_food_insecurity.R` when the data is
for LAC

## 0.1.10.0 (22 July 2024)

### idmc_displacement

- Implemented new method to monitor with the update to `{idmc}` and adjusted signals
thresholds for displacement
- All signals re-generated with the new methods

### System

- Setup `HS_FIRST_RUN` and `HS_LOCAL` to be passed to `generate_signals()` as
environment variables

## 0.1.9.0 (17 July 2024)

### Documentation

- Published the AI summarization prompts in the GitBook, and automated publishing
whenever prompts are edited or added in `src/indicators`
- Small documentation fix in `cloud_storage.R`

### System

- Add functionality to automatically read Excel files from cloud storage in `read_az_file()`
- Added custom location name handling for `C. African Rep.`
- Implemented `input/asap_iso3.parquet` mapping of ASAP codes to ISO3 codes
- Added `asap_to_iso3()` function `location_codes.R`

### jrc_agricultural_hotspots

- Use `asap_to_iso3()` mapping instead of name mapping in `raw_agricultural_hotspots.R`

### System

- Implement `HS_DRY_RUN` and `HS_FIRST_RUN` environment variables to determine
how `generate_signals()` and its helpers create signals
- Use `HS_FIRST_RUN` to ensure that all plots in the first run are filtered to
the date of the alert
- Change `triage_signals()` to accept the `test` argument rather than `dry_run`
to avoid confusion with the env variable

## 0.1.8.0 (16 July 2024)

### jrc_agricultural_hotspots

- Changed AI summarization prompt to ensure no formatting included in output

### wfp_market_monitor

- Changed caption to plot slightly to use the wording "food baskets"

## 0.1.7.0 (11 July 2024)

### idmc_displacement

- Moved to new version of [idmc](https://github.com/OCHA-DAP/idmc) that filters
out potentially duplicated entries in the IDU

### System

- Added automatic checks to ensure the Signals version is incremented and recorded
in this file

## 0.1.6.1 (10 July 2024)

### System

- Added ability to archive signals without sending them in the rare case there is
a decision not to send out a signal

## 0.1.6.0 (9 July 2024)

### System

- Added single quotes ' to trimming rules from AI summary outputs

### Visuals

- Ensured that city labels on maps is less likely to overlap with city points
- Added yearly breaks to time series plots if visual spans 3 or more years

### wfp_market_monitor

- Changed the email text based on comments from WFP

### ipc_food_insecurity

- Fixed links in the 'Further information' section

## 0.1.5.0 (4 July 2024)

### who_cholera
- Cholera monitoring script updated to run on the Mailchimp system, with a new
automatic monitoring worfklow to match that for the other, subscribable datasets

## 0.1.4.1 (3 July 2024)

### System

- Setup weekly backup of Mailchimp audience

## 0.1.4.0 (2 July 2024)

### Data
- Data dictionary for Signals CSV created and added to HDX

### System

- Turned off link tracking for new campaigns to prevent email quarantining

## 0.1.3.1 (1 July 2024)

### System

- Slack alerts for new signals now contain # of recipients for the signal

## 0.1.3.0 (27 June 2024)

### ipc_food_insecurity

- Removed points from map legend for phases to avoid confusion
- Improved `ch_sit_rep.txt` and `ipc_sit_rep.txt` summarization prompts

## 0.1.2.1 (24 June 2024)

### Documentation
- Added documentation on static assets in Azure and new approach to versioning

## 0.1.2.0 (20 June 2024)

### Maps
- New country boundaries spatial data used for locations that do not
have COD boundaries, replacing the simplified boundaries used previously
- Added country labels for single multilocation IPC map (Nicaragua, El Salvador,and Honduras)

### System
- ISO3 codes added to the location boundary GeoJSON files
- Added additional level to semantic versioning, `0.0.0.X`, to be used
for patch changes that do not impact the signals, such as changes to documentation
or changes like the above (adding ISO3 codes to internal files for our convenience)
- Pass `HS_HDX_BEARER` env var to `update_assets.yaml` action

## 0.1.1 (11 June 2024)

### Email
- Fix grammatical error in the footer text linking to the HDX website

## 0.1.0 (10 June 2024)

- Initial release of HDX Signals.
