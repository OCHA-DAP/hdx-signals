## Code infrastructure

All code for generating the alerts is contained within `src`. The structure of
the `src` folder looks like the below:

```
├── email
│   ├── email.Rmd
│   └── email_subsection.Rmd
|
├── indicators
│   ├── indicator1
│   │   ├── README.md
│   │   └── update_indicator1.R
│   └── indicator2
│       ├── README.md
│       └── update_indicator2.R
|
├── utils
│   ├── utils1.R
│   └── utils2.R
|
└── send_alerts.R
```

* [`src/email`](/src/email): contains all RMarkdown files for generating email alerts.
* [`src/indicators`](/src/indicators): contains code for updating individual indicator data, where
each indicator has a folder with documentation and an `update_(...).R` script.
* [`src/utils`](/src/utils): has a set of utilities that are used across various scripts, for
reading and writing to the Google Cloud Storage bucket, sending emails, generating flags, and
other common tasks.
* [`send_alerts.R`](/src/ALERTS.md) is the overall alerting script that pulls together updated
data and generates email alerts when necessary.

## Data storage

Data is stored within a Microsoft Azure Data Storage container, `hdx-signals` on
the Centre for Humanitarian data blob. Access
to the bucket is provided through SAS authorization. This needs to be provided
as an environment variable `DSCI_AZ_SAS`, with the endpoint provided as
`DSCI_AZ_ENDPOINT`.

Legacy file saving to Google Drive and Sheets is still maintained, using a
service account with an authorization file that needs to be saved to
`HDX_SIGNALS_JSON` environment variable. Once this is done, the code,
particularly that to save to and read from the bucket in `google_drive.R`
works on personal machines for testing and in GitHub Actions.

## Automation

Analysis is re-run nightly using
[GitHub Actions](.github/workflows/global-monitoring.yaml). All of the indicator
`update_(...).R` files are run first, one by one, in a bash script, loading in
the latest indicator data. Then `send_alerts.R` is run, pulling in this latest
data and for any new flags, building and sending out emails.


## {box}

The [{box}](https://klmr.me/box/) package is used to manage libraries and local
modules used for global monitoring. This replaces using `library()` calls for
global libraries and `source()` for local modules, instead moving to a modular
approach akin to Python where `box::use(library)` makes a libraries exports
available using `library$function()` and local modules importable using relevant
paths from the calling environment, where `box::use(path/module)` refers to
`module.R` and allows you to call exports from that module with `module$function()`.

