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

# HDX CERF data exploration

A brief exploration of the HDX CERF data, to select the columns
that are then put into `utils`

```python
%load_ext jupyter_black
```

```python
import pandas as pd
import altair as alt
```

```python tags=[]
# needed to plot dataframes with Altair of more than 5000 rows
alt.data_transformers.enable("data_server")
```

```python
filename_hdx = "../data/input/Data_ CERF Donor Contributions and Allocations - allocations.csv"
filename_nicolas = "../data/input/220413 CERF - all applications.xlsx"
```

```python
df_cerf_hdx = pd.read_csv(filename_hdx)
df_cerf_hdx["dateUSGSignature"] = pd.to_datetime(
    df_cerf_hdx["dateUSGSignature"]
)
df_cerf_hdx = df_cerf_hdx[df_cerf_hdx["dateUSGSignature"].dt.year >= 2016]
df_cerf_nicolas = pd.read_excel(filename_nicolas, header=1).rename(
    columns={
        "Total Amount Requested\n(for the overall response, "
        "not just CERF)": "Amount Requested"
    }
)
df_cerf_nicolas["Date of ERC Endorsement"] = pd.to_datetime(
    df_cerf_nicolas["Date of ERC Endorsement"]
)
```

```python
# Compare the columns of the two datasets
df_cerf_hdx.columns
```

```python
df_cerf_nicolas.columns
```

```python
# Compare the emergency types. They seem the same.
sorted(df_cerf_hdx.emergencyTypeName.unique())
```

```python
sorted(df_cerf_nicolas["Emergency Type"].unique()[:-1])
```

```python
# Try limiting to the emergency types given by Nicolas
em_types = [
    # Most certain to be related to conflict
    "Displacement",
    "Refugees",
    # May have displacement component
    "Human Rights",
    "Post-conflict Needs",
    "Violence/Clashes",
    "Multiple Emergencies",
]
```

```python
df_cerf_hdx_em = df_cerf_hdx[
    df_cerf_hdx["emergencyTypeName"].isin(em_types)
].sort_values(by="totalAmountApproved", ascending=False)
```

```python tags=[]
df_cerf_hdx_em
```

```python
# Try limiting Nicolas dataset to the funds that were given to
# IDPs, refugees, and returnees
df_cerf_nicolas_people = df_cerf_nicolas[
    (df_cerf_nicolas["Number of Refugees"] > 0)
    | (df_cerf_nicolas["Number of IDPs"] > 0)
    | (df_cerf_nicolas["Number of Returnees"] > 0)
].sort_values(by="Amount Approved", ascending=False)
```

```python
df_cerf_nicolas_people
```

```python
# Also limit Nicolas by emergency
df_cerf_nicolas_em = df_cerf_nicolas[
    df_cerf_nicolas["Emergency Type"].isin(em_types)
].sort_values(by="Amount Approved", ascending=False)
```

```python
df_cerf_nicolas_em
```

```python
chart1 = (
    alt.Chart(df_cerf_hdx_em)
    .mark_point()
    .encode(
        x="dateUSGSignature",
        y="countryName",
        color=alt.Color(
            "totalAmountApproved",
            scale=alt.Scale(type="log"),
        ),
    )
)

chart2 = (
    alt.Chart(df_cerf_nicolas_em)
    .mark_point(shape="wedge")
    .encode(
        x="Date of ERC Endorsement",
        y="Country",
        color=alt.Color(
            "Amount Requested",
            scale=alt.Scale(type="log"),
        ),
    )
)

chart3 = (
    alt.Chart(df_cerf_nicolas_people)
    .mark_point(shape="diamond")
    .encode(
        x="Date of ERC Endorsement",
        y="Country",
        color=alt.Color(
            "Amount Requested",
            scale=alt.Scale(type="log"),
        ),
    )
)


lc = chart1 + chart2 + chart3
lc.configure_axisX(labelAngle=90).configure_axisY(grid=True)
```
