# Explore IDMC data

This notebook is for a simple exploration of the IDMC data

```python
%load_ext jupyter_black
```

```python
import requests

import pandas as pd
import hvplot.pandas

from src import utils
```

```python
df_idmc = utils.get_idmc_data()
```

```python
# Look at displacement typees
df_idmc.displacement_type.unique()
```

```python
# Let's only take conflict for now
df_idmc = df_idmc.loc[df_idmc["displacement_type"] == "Conflict"]
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
