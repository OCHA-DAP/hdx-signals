# CERF Global Monitoring System

This repository contains the code base for the CERF Global Monitoring System (GMS).

## Introduction

The CERF GMS is an early warning system designed to generate alerts on
potentially new or worsening crises. Alerts are intended to be just an initial
signal that a crisis should be further explored and see if action, such as
rapid response, is justified.

## Analysis

Early warnings are run from different sets of global indicators. All of the
analysis code is contained under the `src` folder. Each indicator has
its own folder with a `update_(...).R` file that downloads raw data for the
indicator, wrangles into a usable file for visualizations and alerts, and
generates flags for that indicator. These files are all standalone files that
generates alerts for that
indicator, as well as a `README.md` file with details on the analysis
specific to that indicator.

All analysis is run using the `src/run.R` file. The structure of the
`src` folder looks like the below:

## Flagging outputs

1. `flags_total.gdsheets`: all total flags generated from the sourced indicators.
Each row corresponds to a flag, with information indicating the country being
flagged, the type and source of the flag, and information related to what
has generated the latest flags.
2. `flags_total_daily.gdsheets`: the same dataset as `flags_total.gdsheets`, but where
each flag has a row for the days between `start_date` and `end_date`. Used for
filtration on the PowerBI dashboard.

## Email records

A record of previous emails generated is stored in `flags_emailed.gdsheets`, which
simply stores records from `flags_total.gdsheets` whenever they are used to generate
an email, with a single additional column recording the date of the email.

----

License: [GPLv3](LICENSE)
