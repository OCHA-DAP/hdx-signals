---
coverY: 0
layout:
  cover:
    visible: false
    size: full
  title:
    visible: true
  description:
    visible: true
  tableOfContents:
    visible: true
  outline:
    visible: true
  pagination:
    visible: true
---

# Overview

HDX Signals monitors key datasets and generates automated emails when significant, negative changes are detected. Signals are identified for given datasets and locations.&#x20;

Over 200 locations are monitored, with coverage varying depending on the dataset. The following table details the number of locations covered by each dataset:&#x20;

| Datasets                               | Number of locations |
| -------------------------------------- | ------------------- |
| Agricultural hotspots                  | 81                  |
| Conflict events                        | 188                 |
| Food insecurity                        | 53                  |
| Internal conflict-driven displacements | 82                  |
| Internal disaster-driven displacements | 199                 |
| Market monitoring                      | 80                  |

We use a thresholds approach to identify when a signal should be created in a given dataset. The specific methodology for each dataset is detailed in the "Datasets" section of this site. We only send a signal if it has been detected in the past sixty days and if no identical signal was detected in the past six months.&#x20;

