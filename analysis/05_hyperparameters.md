---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.13.7
  kernelspec:
    display_name: global-monitoring
    language: python
    name: global-monitoring
---

# Hyperparameters

How does changing the following hyperparameters impact the result:

- fraction of anomalies
- K (in KNN)

Can also look into some
[here](https://scikit-learn.org/stable/modules/generated/sklearn.neighbors.KNeighborsClassifier.html),
e.g.:

- distance metric

```python
%load_ext jupyter_black
```

```python
import altair as alt
import pycaret.anomaly as pa
import pandas as pd
from altair import datum

from src import utils, constants
```

```python
# needed to plot dataframes with Altair of more than 5000 rows
alt.data_transformers.enable("data_server")
```

```python tags=[]
# Get model data, conflict for now
df_idmc = utils.get_idmc_data()
df_idmc = df_idmc.loc[df_idmc["displacement_type"] == "Conflict"]
df_idmc_model = utils.get_model_input_data(df_idmc)
```

## Anomaly fraction

```python tags=[]
# Run for different fractions
anom = pa.setup(
    data=df_idmc_model,
    silent=True,
    ignore_features=["id"],
)
df_results = pd.DataFrame()
fractions = [0.005, 0.01, 0.02, 0.03, 0.04, 0.05]
for fraction in fractions:
    anom_model = pa.create_model(model=constants.model, fraction=fraction)
    results = pa.assign_model(anom_model)
    results["fraction"] = fraction
    df_results = pd.concat([df_results, results], ignore_index=True)

df_final = df_results.merge(
    df_idmc,
    on="id",
    how="left",
    copy=False,
    suffixes=[None, "_todrop"],
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
        y="fraction:O",
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
        y="fraction:O",
    )
    .transform_filter((datum.Anomaly == 0))
)

lc = chart2 + chart1

lc.configure_axisX(labelAngle=90)
```

We can see that for Myanmar, we would need a fraction of at least
3% to have detected the anomaly in April, and 2% for July.

## K (number of nearest neighbours)

```python
# Run for different fractions
anom = pa.setup(
    data=df_idmc_model,
    silent=True,
    ignore_features=["id"],
)
df_results = pd.DataFrame()
ks = [2, 3, 4, 5, 10, 20, 30]
for k in ks:
    anom_model = pa.create_model(model=MODEL, n_neighbors=k)
    results = pa.assign_model(anom_model)
    results["k"] = k
    df_results = pd.concat([df_results, results], ignore_index=True)

df_final = df_results.merge(
    df_idmc,
    on="id",
    how="left",
    copy=False,
    suffixes=[None, "_todrop"],
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
        y="k:O",
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
        y="k:O",
    )
    .transform_filter((datum.Anomaly == 0))
)

lc = chart2 + chart1

lc.configure_axisX(labelAngle=90)
```

Hm so now the question, how do you tune hyperparameters
for unsupervised anomaly detection? Briefly checking the
literature I found a paper
[here](https://github.com/albertcthomas/anomaly_tuning),
to look into later. Will leave k at 5 for now.

```python

```
