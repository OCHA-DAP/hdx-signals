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

# Visulaization

Some viz for the first CERF meeting

```python
%load_ext jupyter_black
```

```python
from pathlib import Path

from datetime import date
import altair as alt
import pandas as pd

from src import utils
```

```python
OUTPUT_DATA_DIR = Path("../data/output")
```

```python
# CERF data
df_cerf = utils.get_cerf_hdx_data()
df_cerf = df_cerf[
    df_cerf["dateUSGSignature"] >= pd.to_datetime(date(2018, 2, 1))
]
```

```python
# Get model data, conflict for now
df_idmc = utils.get_idmc_data()
df_idmc = df_idmc.loc[df_idmc["displacement_type"] == "Conflict"]
df_idmc_model = utils.get_model_input_data(df_idmc)
df_final = utils.run_model(df_idmc, df_idmc_model)
```

```python
def plot_country(country, iso3):

    base_idmc = (
        alt.Chart(df_final)
        .mark_circle()
        .encode(
            x=alt.X("displacement_date", title="Displacement date"),
            y=alt.X(
                "figure",
                title="Displaced persons",
            ),
            color=alt.Color("Anomaly:N", legend=None),
            tooltip=["standard_popup_text"],
        )
        .properties(height=100, width=400)
    ).configure_axisX(labelAngle=90)

    base_cerf = (
        alt.Chart(df_cerf)
        .mark_point(shape="wedge", color="green", angle=180)
        .encode(x="dateUSGSignature", y="amount")
    )

    chart = (
        (
            base_idmc.transform_filter(alt.datum.iso3 == iso3)
            # + base_cerf.transform_filter(alt.datum.countryCode == "MMR")
        ).properties(title=country)
        # .resolve_scale(y="independent")
    )
    chart
    chart.save(OUTPUT_DATA_DIR / f"{country}.png", scale_factor=2)
```

```python
plot_country("Myanmar", "MMR")
```

```python
plot_country("Afghanistan", "AFG")
```

```python
plot_country("Sudan", "SDN")
```

```python
plot_country("Ethiopia", "ETH")
```

```python
df_final["Anomaly"] = df_final["Anomaly"].replace({0: False, 1: True})
df_final.rename(
    columns={
        "id": "idmc_id",
        "figure": "n_displaced",
        "Anomaly": "anomalous",
    }
).drop(
    columns=["Anomaly_Score", "pop", "day_of_year", "year", "duration"]
).to_excel(
    OUTPUT_DATA_DIR / "global_monitoring_knn_model_results.xlsx", index=False
)
```

```python

```
