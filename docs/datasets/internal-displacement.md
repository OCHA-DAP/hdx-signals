# Internal displacements

Signals are sent when monthly displacements reach levels not seen in the past one to three years.

## Source

We monitor the [Internal Displacement Monitoring Centre's (IDMC) ](https://www.internal-displacement.org/)event data on conflict and disaster-driven population flows. Access IDMC data directly on [HDX](https://data.humdata.org/organization/international-displacement-monitoring-centre-idmc) and find additional datasets and context on the [IDMC website](https://www.internal-displacement.org/).

## Signals detection

We first remove duplicated event databased on event ID. If multiple entries for a single event ID, then only the recommended figures are kept if available. If unavailable, then the latest data updated into the IDU is kept based on the `created_at` date. We then transform this IDMC event data into a time series and calculate monthly displacements as the rolling sum of displacements across the past 30 days.

For some countries, we only consider reported events from a certain date when we are confident in consistent coverage. This is because the IDMC IDU has uncertain temporal coverage. This temporary fix will be removed once the IDMC provides metadata indicating the start date for coverage for each country. You can find the dates we use in [this table](displacement-dates.md).

Signals are generated whenever monthly displacements reach levels not seen in the past one or three years. Given the significant differences in conflict and natural disaster displacement, signals are generated separately for each type. Signals are not generated if monthly fatalities are below the minimum threshold of 5,000 (conflict-driven) and 50,000 (disaster-driven).

#### Examples

[Philippines](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=edd0633c44): Approximately 45,000 conflict-driven displacements were reported from February to March 2021.

[Congo](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=ee643f5da7): Floods in late 2023 causes large-scale displacement.
