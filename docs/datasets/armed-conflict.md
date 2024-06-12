# Armed Conflict

Signals are sent when monthly conflict levels reach levels not seen in the past one to three years.

## Source

We monitor [Armed Conflict Location & Event Data Project (ACLED)](https://acleddata.com) data. ACLED data contains dates, actors, locations, fatalities, and types of all reported political violence and protest events in more than [200 locations in real time](https://acleddata.com/download/4404/). You can access aggregated ACLED data directly on [HDX](https://data.humdata.org/organization/acled?) and get full, disaggregated data through the [ACLED data and tools](https://acleddata.com/data/).

## Signals detection

We transform ACLED event datasets into a time series of armed conflict and fatalities, and calculate monthly fatalities as the rolling sum of fatalities across the past 30 days. Signals are generated whenever monthly fatalities reaches levels not seen in the past one or three years. Signals are not generated if monthly fatalities are below the minimum threshold of 100.

#### Examples

Below are historical examples that show how signals are generated with ACLED data.

[Myanmar](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=3b6173e87a): in February 2021, monthly fatalities reached 169, higher than any time in the past year.

[South Sudan](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=171a61325b): in June 2022, monthly fatalities reached 527, a level not seen since early 2019.
