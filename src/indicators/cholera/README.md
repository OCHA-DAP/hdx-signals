# Cholera

Cholera data is scraped by CERF from
[WHO AFRO bulletins](https://www.afro.who.int/publications/outbreaks-and-emergencies-bulletin-week-8-13-19-february-2023-cloned).
The reports for the Africa region contain outbreaks for countries, with start
dates and end dates for each outbreak incident.

## Introduction

The data is stored on the CERF servers. Access to it should be provided by link,
stored as an environment variable `CERF_CHOLERA_DATA`. The data for cases is
reported cumulatively during an outbreak, so cases should generally only
be increasing from the reported start date.

## Methodology

The methodology for its use for global monitoring is below.

### Tidy the data

Simple wrangling is done to the data, with column names cleaned and the correct
start dates for outbreaks classified. There are a few instances of missing data
from the middle of outbreaks, and those cases are infilled linearly. As well,
some countries have 2 separate reports of cases each week, such as the DRC. In
this instance, we combine them into a single weekly value. Data is cumulative
from the start of an outbreak, with some exceptions due to presumed reporting
errors.

### Flagging

Flags are generated when the reported cases in a week cross a specific threshold
and the threshold was not met in the previous week. The thresholds are 1,000,
2,000, 5,000, and 10,000 cases. For each flag, the start date is the onset
date of the outbreak. The end date is the date that the flag was raised.

## Output datasets
 
`flags_cholera.csv`: flagging dataset.

`raw_cholera.csv`: raw cholera output from the CERF scraping.

`wrangled_cholera.csv`: wrangled cholera data that has a clean time series
of cholera casees.
