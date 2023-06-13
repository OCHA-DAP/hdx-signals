# CERF Global Monitoring System

This repository contains the code base for the CERF Global Monitoring System (GMS).

## Introduction

The CERF GMS is an early warning system designed to generate alerts on
potentially new or worsening crises. Alerts are intended to be just an initial
signal that a crisis should be further explored and see if action, such as
rapid response, is justified.

## Analysis

These early warnings are run on different global indicators. All of the
analysis code is contained under the `src` folder. Each indicator has
its own folder with a `run.R` file that generates the alerts for that
indicator, as well as a `README.md` file with details on the analysis
specific to that indicator.

All analysis is run using the `src/run.R` file. The structure of the
`src` folder looks like the below:

```
├── indicators
│   ├── indicator1
│   │   ├── README.md
│   │   └── run.R
│   └── indicator2
│       ├── README.md
│       └── run.R
└── run.R
```

Analysis is re-run nightly using GitHub Actions. More details will come on
what is needed to reproduce the analysis.

## Flagging outputs

1. `flags_total.csv`: all total flags generated from the sourced indicators.
Each row corresponds to a flag, with information indicating the country being
flagged, the type and source of the flag, and information related to what
has generated the latest flags.
2. `flags_total_daily.csv`: the same dataset as `flags_total.csv`, but where
each flag has a row for the days between `start_date` and `end_date`. Used for
filtration on the PowerBI dashboard.

## Email records

A record of previous emails generated is stored in `flags_emailed.csv`, which
simply stores records from `flags_total.csv` whenever they are used to generate
an email, with a single additional column recording the date of the email.

## Indicators

The methodologies for each of the indicators analyzed within the project
are contained in their own specific README files, linked below. The code
output for all data sources are three files:

1. `flags_{type}.csv`: a flagging dataset that has a row of flags
with a start date, end date, and explanatory message. There is only one
output flag for each source of data.
2. `raw_{type}.csv`: the raw data from the data source that can be used to
contextualize the alert. Typically only useful in tabular format.
3. `wrangled_{type}.csv`: the wrangled data from the data source that can
be used to contextualize the alert, and is generally ready for plotting.

The current indicators included in the CERF GMS are:

- [Internal Displacement Monitoring Centre (IDMC) displacement data](src/indicators/idmc/README.md)
- [Integrated Food Security Phase Classification (IPC) food security data](src/indicators/ipc/README.md)
- [WHO AFRO cholera data](src/indicators/cholera/README.md)

----

License: [GPLv3](LICENSE)
