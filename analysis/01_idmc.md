# Explore IDMC data

This notebook is for a simple exploration of the IDMC data

```python
%load_ext jupyter_black
```

```python
from datetime import date

import altair as alt
import pandas as pd
import hvplot.pandas

from src import utils
```

```python
# needed to plot dataframes with Altair of more than 5000 rows
alt.data_transformers.enable("data_server")
```

```python
df_idmc = utils.get_idmc_data()
df_idmc
```

```python
df_idmc_plot = df_idmc[
    (df_idmc["figure"] > 0)
    & (df_idmc["displacement_date"].dt.year > 2017)
    & (df_idmc["displacement_date"] < pd.to_datetime(date(2022, 5, 1)))
]
alt.Chart(df_idmc_plot).mark_point().encode(
    x=alt.X("displacement_date", title="Dislpacement Date"),
    y=alt.Y(
        "figure",
        title="Displaced persons",
    ),  # scale=alt.Scale(type="log")),
    color=alt.Color(
        "displacement_type", legend=alt.Legend(title="Displacement type")
    ),
).configure_axisX(labelAngle=90)
```

```python
# Look at displacement typees
df_idmc.displacement_type.unique()
```

```python
# Let's only take conflict for now
df_idmc = df_idmc.loc[df_idmc["displacement_type"] == "Conflict"]
len(df_idmc)
```

```python
# Check which countries are missed by the pop data
missing_countries = df_idmc.loc[df_idmc["pop"].isna()]["country"].unique()
# Note that the following countries don't have pop data.
# Can add by hand if deemed necessary
print(missing_countries)
```

```python
# Drop the missing countries
df_idmc.dropna(subset="pop")
# Calculate figure per 100,000 people
df_idmc["figure_per_capita"] = df_idmc["figure"] / df_idmc["pop"] * 100000
```

```python
idx = pd.date_range("2016-01-01", "2022-04-01")
df_clean = (
    df_idmc.groupby(["iso3", "displacement_date"])
    .sum()[["figure_per_capita"]]
    .reset_index()
    .pivot(
        index="displacement_date", columns="iso3", values="figure_per_capita"
    )
    .fillna(0)
    .reindex(idx, fill_value=0)
    .rolling(10)
    .sum()
)
```

```python
df_clean.hvplot()
```

```python
df_clean["MMR"].hvplot()
```
