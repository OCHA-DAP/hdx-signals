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

# Anomaly fraction

How does changing the fraction of anomalies impact the result

```python
%load_ext jupyter_black
```

```python
import pycaret.anomaly as pa

from src import utils
```

```python
# Build model -- this should probably go into utils

df_idmc = utils.get_idmc_data()
df_idmc = df_idmc.loc[df_idmc["displacement_type"] == "Conflict"]
# Make year, day, and duratoin columns
df_idmc["day_of_year"] = df_idmc["displacement_date"].dt.dayofyear
df_idmc["duration"] = (
    df_idmc["displacement_end_date"] - df_idmc["displacement_start_date"]
).dt.days
columns = [
    "id",
    "latitude",
    "longitude",
    "figure",
    "day_of_year",
    "year",
    "duration",
    "pop",
]
df_idmc_model = df_idmc[columns].dropna()
```

```python

```
