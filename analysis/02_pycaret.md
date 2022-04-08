# Unsupervised anomaly detection

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
df_idmc = utils.get_idmc_data()
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
columns = [
    "id",
    "latitude",
    "longitude",
    "figure",
    "day_of_year",
    "year",
    "duration",
    "event_duration",
    "event_time_diff",
    "pop",
]
df_idmc_model = df_idmc[columns].dropna()
```

```python
anom = pa.setup(data=df_idmc_model, silent=True, ignore_features=["id"])
```

```python
models
```

```python
models = list(pa.models().index)[:-1]
```

```python
df_results = pd.DataFrame()
fraction = 0.05
for model in models:
    print(model)
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
df_final[(df_final["iso3"] == "MMR") & (df_final["Anomaly"] == 1)]
```

```python
alt.Chart(df_final[df_final["iso3"] == "MMR"]).mark_point().encode(
    x="displacement_date", y="model", color="figure"
).transform_filter(
    (datum.Anomaly == 1) & (datum.iso3 == "MMR")
).configure_axisX(
    labelAngle=90
)
```

```python
alt.Chart(df_final[df_final["Anomaly"] == 1]).mark_point().encode(
    x="displacement_date", y="iso3", color="figure"
).transform_filter((datum.model == "abod")).configure_axisX(
    labelAngle=90
).configure_axisY(
    grid=True
)
```
