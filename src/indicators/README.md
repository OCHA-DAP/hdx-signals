## Indicators

Different core datasets are scanned for signals. Each indicator has
its own folder with a `update_{...}.R` file that downloads raw data for the
indicator, wrangles into a usable file for visualizations and alerts, and
generates signals. Utilities for wrangling, analyzing, and creating signals content
is contained in a `utils` folder. Prompts for AI summarization are in a `prompts`
folder.

Each `update_{...}.R` script is run automatically in GitHub Actions. When signals
are detected, an `output/{indicator_id}/signals.parquet` file is created that can
then be triaged into the overall Signals dataset.

Methodologies for signalling for each dataset are specified in the public-facing
Gitbook documentation. The exceptions are for those indicators that are not
publicly subscribable. These are documented in their
`src/indicators/{indicator_id}` folder:

- [who_cholera](who_cholera/README.md)
