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
```

```python
filename = "../data/input/Data_ CERF Donor Contributions and Allocations - allocations.csv"
```

```python
df = pd.read_csv(filename)
```

```python
df.emergencyTypeName.unique()
```

```python
em_types = [
    "Multiple Emergencies",
    "Post-conflict Needs",
    "Displacement",
    "Unspecified Emergency",
    "Human Rights",
    "Economic Disruption",
    "Violence/Clashes",
    "Refugees",
]
```

```python
df = df[df["emergencyTypeName"].isin(em_types)].sort_values(
    by="totalAmountApproved", ascending=False
)
```

```python
df
```
