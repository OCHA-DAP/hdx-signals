## Indicators

Different core datasets are scanned for signals. 

Each indicator has its own folder, generally with the following structure:

├── __init__.R
├── prompts
│   ├── short.txt
│   └── ...
├── utils
│   ├── {ind}_raw.R
│   ├── {ind}_wrangle.R
│   └── ...

The summary prompts for the indicator are stored in `prompts` and called in `summary.R`
for that indicator. The functions that download the data, wrangle it, create alerts,
and generate the content are all stored in the `utils` folder.

The `__init__.R` file for the indicator module should re-export all of the functions
stored in `utils`, ensuring that functions like `raw()`, `wrangle()`, etc. are all
present. It should also define `indicator_id` as a string exported with the module.

In this way, all indicator modules have identical `functions()` exported,
so they behave extremely similarly to classes and methods. It also means the entire
module can be passed to `generate_signals()`.

There should then be code at the end of the `__init__.R` script so that if the script
is sourced or called (not imported as a box module), then `generate_signals()` is run
for that indicator. Each `__init__.R` script is run automatically in GitHub Actions. 

When signals are detected, an `output/{indicator_id}/signals.parquet` file is created that can
then be triaged into the overall Signals dataset.

Methodologies for signalling for each dataset are specified in the public-facing
Gitbook documentation. The exceptions are for those indicators that are not
publicly subscribable. These are documented in their
`src/indicators/{indicator_id}` folder:

- [who_cholera](who_cholera/README.md)
