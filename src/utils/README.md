## Utilities

Useful utilities used across `src` and `src-static`.

- [`add_locations_metadata.R`](add_locations_metadata.R): Add location metadata
(e.g. country names) to a data frame by joining by ISO3 codes.
- [`ai_summarizer.R`](ai_summarizer.R): Wrap the OpenAI API and generate AI summaries
from text information and prompts.
- [`alert_daily_ts.R`](alert_daily_ts.R): Generate alerts data frame from daily 
time series data.
- [`all_iso3_codes.R`](all_iso3_codes.R): Get all ISO3 codes for locations covered
by HDX Signals.
- [`cloud_storage.R`](cloud_storage.R): Read from and write to the Azure blob
storage.
- [`download_shapefile.R`](download_shapefile.R): Download shapefile from a URL.
- [`formatters.R`](formatters.R): Format dates in standard format for use in
communications. Previously had other formatters, which may be added in the future.
- [`get_env.R`](get_env.R): Check if environment variable defined, and throw error
if not. Can return the value if necessary.
- [`get_iso3_sf.R`](get_iso3_sf.R): Get spatial data for an ISO3 code, such as ADM0
boundaries, centroids, or cities.
- [`get_prompts.R`](get_prompts.R): Get AI text summarization prompts from raw
text files stored in `indicators/{indicator_id}/prompts`.
- [`get_signals_version.R`](get_signals_version.R): Retrieve current version of
Signals from `.signals_version`.
- [`gmas_test_run.R`](gmas_test_run.R): Check if the current setup is for a testing
dry run or not.
- [`hs_logger.R`](hs_logger.R): Configure logging environment.
- [`iso3_shift_longitude.R`](iso3_shift_longitude.R): Shift longitude for specific
ISO3 codes that have data crossing the antimeridian.
- [`location_codes.R`](location_codes.R): Utilities to get location names from ISO3 codes
and get ISO3 codes from a variety of other codes/naming conventions without generating
warnings.
- [`parse_pdf.R`](parse_pdf.R): Parse PDF from URL and extract text.
- [`push_hdx.R`](push_hdx.R): Push data to HDX by calling the repository webhook.
- [`temp_file.R`](temp_file.R): Create temporary files locally in the repository
to local testing of emails display properly in browsers.
- [`update_coverage.R`](update_coverage.R): Update coverage dataset for an indicator,
used to constantly record what ISO3 codes are covered in a dataset.
