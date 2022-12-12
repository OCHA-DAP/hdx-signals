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

# Baseline model

In this notebook we try to come up with a simple model to compare
with the unsupervised learning one

```python
%load_ext jupyter_black
```

```python
import pandas as pd
from statsmodels.tsa.seasonal import seasonal_decompose
import altair as alt
import hvplot.pandas

from src import utils
```

```python
# needed to plot dataframes with Altair of more than 5000 rows
alt.data_transformers.enable("data_server")
```

```python
df_idmc = utils.get_idmc_data()
# Let's only take conflict for now
df_idmc = df_idmc.loc[df_idmc["displacement_type"] == "Conflict"]
```

```python
# Drop the missing countries
df_idmc.dropna(subset="pop")
# Calculate figure per 100,000 people
df_idmc["figure_per_capita"] = df_idmc["figure"] / df_idmc["pop"] * 100000
```

```python
idx = pd.date_range("2016-01-01", "2022-04-01")
df_idmc_daily = (
    df_idmc.groupby(["iso3", "displacement_date"])
    .sum()[["figure_per_capita"]]
    .reset_index()
    .pivot(
        index="displacement_date", columns="iso3", values="figure_per_capita"
    )
    .fillna(0)
    .reindex(idx, fill_value=0)
    .rolling(31)
    .sum()
    .fillna(0)
)
```

```python
df_idmc_daily
```

```python
# Try using stats model decomposition
# https://otexts.com/fpp2/decomposition.html
series = df_idmc_daily["YEM"]
result = seasonal_decompose(series, model="additive")
result.plot()
```

```python
# Looks pretty useless, also for other countries.
# Try some kind of threshold, maybe 1000
```

```python
# First need to make a long form chart for alt
def get_long_form(df_in):
    df = df_in.copy()
    df["date"] = df.index
    df = df.melt(id_vars="date")
    df["value"] += 1  # for logging
    return df


df_idmc_alt = get_long_form(df_idmc_daily)
```

```python
threshold = 200  # Threshold needed to get MMR in April 2021

chart1 = (
    alt.Chart(df_idmc_alt[df_idmc_alt["value"] > threshold])
    .mark_point(shape="wedge")
    .encode(
        x="date",
        y="iso3",
        color=alt.Color(
            "value",
            # scale=alt.Scale(type="log"),
        ),
    )
)

chart1
```

```python
# Make a version that divides out the median
def sub_median(x):
    median = x.median()
    # median = np.where(median, median == 0, 1)
    return x - median


grouper = pd.Grouper(freq="1Y")
df_idmc_daily_noseas = df_idmc_daily.groupby(grouper).transform(
    lambda x: sub_median(x)
)
df_idmc_noseas_alt = get_long_form(df_idmc_daily_noseas)
df_idmc_daily_noseas.hvplot()
```

```python
threshold = 100  # Threshold needed for MMR in April
chart1 = (
    alt.Chart(df_idmc_noseas_alt[df_idmc_noseas_alt["value"] > threshold])
    .mark_point(shape="wedge")
    .encode(
        x="date",
        y="iso3",
        color=alt.Color(
            "value",
            # scale=alt.Scale(type="log"),
        ),
    )
)

chart1
```
