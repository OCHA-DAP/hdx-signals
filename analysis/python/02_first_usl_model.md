# Unsupervised anomaly detection

Use PyCaret to try out different unsupervised anomaly detection methods

```python
%load_ext jupyter_black
```

```python
# from sklearn.model_selection import train_test_split
import pycaret.anomaly as pa
import pandas as pd
import altair as alt
from altair import datum

from src import utils
```

```python
# needed to plot dataframes with Altair of more than 5000 rows
alt.data_transformers.enable("data_server")
```

```python
df_idmc = utils.get_idmc_data()
df_cerf = utils.get_cerf_hdx_data()
df_cerf = df_cerf[
    df_cerf["dateUSGSignature"] > df_idmc["displacement_date"].min()
]
```

```python
# Let's only take conflict for now
df_idmc = df_idmc.loc[df_idmc["displacement_type"] == "Conflict"]
```

```python
# Look at columns to make a training set
df_idmc.columns
```

```python
# Make year, day, and duratoin columns
df_idmc["day_of_year"] = df_idmc["displacement_date"].dt.dayofyear
df_idmc["duration"] = (
    df_idmc["displacement_end_date"] - df_idmc["displacement_start_date"]
).dt.days
df_idmc["event_duration"] = (
    df_idmc["event_end_date"] - df_idmc["event_start_date"]
).dt.days
df_idmc["event_time_diff"] = (
    df_idmc["event_start_date"] - df_idmc["displacement_start_date"]
).dt.days
```

```python
# Start with columns that are numeric
# add other displacment types
columns = [
    "id",
    "latitude",
    "longitude",
    "figure",
    "day_of_year",
    "year",
    "duration",
    # Removing these because almost 3000 datapoints don't have this info,
    # see next cell
    # "event_duration",
    # "event_time_diff",
    "pop",
]
df_idmc_model = df_idmc[columns].dropna()
```

```python
# Check how many missing values due to dropping NA
for cname in columns:
    print(cname, sum(df_idmc[cname].isna()))
# 114 are NA due to requiring lat and lon, could consider
# using country name instead, but will do this in another notebook
```

```python
anom = pa.setup(data=df_idmc_model, silent=True, ignore_features=["id"])
# Remove the last model because it gives an error
models = list(pa.models().index)[:-1]
```

```python
# Loop through the models and get the results
df_results = pd.DataFrame()
fraction = 0.05
for model in models:
    anom_model = pa.create_model(model=model, fraction=fraction)
    results = pa.assign_model(anom_model)
    results["model"] = model
    df_results = pd.concat([df_results, results], ignore_index=True)
```

```python
df_final = df_results.merge(
    df_idmc, on="id", how="left", copy=False, suffixes=[None, "_todrop"]
)
df_final = df_final[
    df_final.columns.drop(list(df_final.filter(regex="_todrop")))
]
```

```python
# Plot results for Myanmar
chart1 = (
    alt.Chart(df_final[df_final["iso3"] == "MMR"])
    .mark_point(opacity=1)
    .encode(
        x="displacement_date",
        y="model",
        color=alt.Color(
            "figure", scale=alt.Scale(type="log", scheme="orangeRed")
        ),
    )
    .transform_filter((datum.Anomaly == 1))
)

chart2 = (
    alt.Chart(df_final[df_final["iso3"] == "MMR"])
    .mark_point(opacity=0.05)
    .encode(
        x="displacement_date",
        y="model",
    )
    .transform_filter((datum.Anomaly == 0))
)

lc = chart2 + chart1

lc.configure_axisX(labelAngle=90)
```

For Myanmar, we believe a reasonable model would have
detected an anomaly around apring of 2021. We can see
that histogram performs quite poorly, as well as SVM.
Alternatively, KNN, which is probably one of the most
well known clustering methods, performs as expected,
so we take it forward.

```python
# Plot all countries using KNN against CERF
chart1 = (
    alt.Chart(df_final[df_final["Anomaly"] == 1])
    .mark_point(opacity=1)
    .encode(
        x="displacement_date",
        y="iso3",
        color=alt.Color(
            "figure", scale=alt.Scale(type="log", scheme="orangeRed")
        ),
    )
    .transform_filter((datum.model == "knn"))
)

chart2 = (
    alt.Chart(df_final[df_final["Anomaly"] == 0])
    .mark_point(opacity=0.1)
    .encode(
        x="displacement_date",
        y="iso3",
    )
    .transform_filter((datum.model == "knn"))
)

chart3 = (
    alt.Chart(df_cerf)
    .mark_point(shape="wedge")
    .encode(
        x="dateUSGSignature",
        y="countryCode",
        color=alt.Color(
            "totalAmountApproved",
            scale=alt.Scale(type="log", scheme="yellowGreen"),
        ),
    )
)

lc = (chart3 + chart2 + chart1).resolve_scale(color="independent")

lc.configure_axisX(labelAngle=90).configure_axisY(grid=True)
```

In general the anomalies don't seem crazy compared to the
CERF data. Some questions remain, for future notebooks:

- which features are important? which can be removed, or changed?
- which hyperparameters should be used? N (for KNN), and fraction of
  anomaly (default 0.05)
- does displacement reporting matter? i.e., if in one country it's tracked
  in detail and many small displacements are reported, while in another
  it's all aggregated together, does that affect the anomaly detection?
- does this model perform "better" than a baseline model? (e.g. a simple threshold)
