# Internal Displacements

Signals are sent when monthly displacements reach levels not seen in the past one to three years.

## Source

We monitor [IDMC's](https://www.internal-displacement.org/) event data on conflict and disaster-driven population flows at the country level.&#x20;

Access IDMC data directly on [HDX](https://data.humdata.org/organization/international-displacement-monitoring-centre-idmc) and find additional datasets and context on the [IDMC website](https://www.internal-displacement.org/).

## Signals detection

We transform IDMC event datasets into a time series and calculate monthly displacements as the rolling sum of displacements across the past 30 days. Signals are generated whenever monthly displacements reach levels not seen in the past one or three years. Given the significant differences in conflict and natural disaster displacement, signals are generated separately for each type. Signals are not generated if monthly fatalities are below the minimum threshold of 10,000 (conflict-driven) and 100,000 (disaster-driven).
