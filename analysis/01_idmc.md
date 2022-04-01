```python
import requests

import pandas as pd
import numpy as np
import hvplot.pandas
```

```python
IDMC_URL = "https://backend.idmcdb.org/data/idus_view_all_flat_cached_ochachd"
POP_FILENAME = "../data/API_SP.POP.TOTL_DS2_en_csv_v2_3731322.csv"
```

```python
df_pop = (pd.read_csv(POP_FILENAME))
```

```python
df_pop.iloc[110]
```

```python
df_idmc[df_idmc["iso3"] == "MMR"]
```

```python
# Get the pop data. Use forward fill to fill missing years for Eritrea.
df_pop = (pd.read_csv(POP_FILENAME)
          .ffill(axis=1) 
          .rename(columns={"2020": "pop", "Country Code": "iso3"})
          .drop(110) # Drop the not classified row, I knwo this is bad
         )[["iso3", "pop"]]
df_pop["pop"] = df_pop["pop"].astype(int) / 100000
```

```python
# Read in the IDMC data, format the date columns
df_idmc = pd.DataFrame(requests.get(IDMC_URL).json())
for colname in ['displacement_date', 
                'displacement_start_date', 
                'displacement_end_date', 
                'event_start_date', 
                'event_end_date']:
    df_idmc[colname] = pd.to_datetime(df_idmc[colname])
```

```python
# Merge the pop data to IDMC data. 
df_idmc_pop = pd.merge(df_idmc, df_pop, how="left", on="iso3")
missing_countries = df_idmc_pop.loc[df_idmc_pop["pop"].isna()]["country"].unique()
# Note that the following countries don't have pop data.
# Can add by hand if deemed necessary
print(missing_countries)
```

```python
df_idmc_pop.dropna(subset="pop")
df_idmc_pop["figure_per_capita"] = df_idmc_pop["figure"] / df_idmc_pop["pop"]

```

```python
idx = pd.date_range('2016-01-01', '2022-04-01')
df_clean = (df_idmc_pop.groupby(['iso3', 'displacement_date'])
 .sum()[['figure_per_capita']]
 .reset_index()
 .pivot(index='displacement_date', columns='iso3', values='figure_per_capita')
 .fillna(0)
 .reindex(idx, fill_value=0)
 .rolling(14).sum()
)
```

```python
df_clean.hvplot()
```

```python
df_clean["MMR"].hvplot()
```
