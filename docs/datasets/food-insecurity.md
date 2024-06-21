# Food Insecurity

Signals are sent when food insecurity is estimated or projected to increase in a new IPC analysis.

## Source

We monitor[ Integrated Food Security Phase Classification (IPC) ](https://www.ipcinfo.org)and [Cadre Harmonisé (CH)](http://cadreharmonise.org) data on acute food insecurity.  IPC/CH data estimate the levels of acute food insecurity in locations, estimating the number and percent of populations that fall into [five phases](https://www.ipcinfo.org/ipcinfo-website/ipc-overview-and-classification-system/ipc-acute-food-insecurity-classification/en/):

1. Minimal/None
2. Stressed
3. Crisis
4. Emergency
5. Catastrophe/Famine

IPC/CH analyses produce current estimates of populations in these phases, as well as future projections on how food insecurity will involve in the coming months. At the subnational level, individual areas are given a phase classification on the same phase scale, which can be used  to [map food insecurity](https://www.ipcinfo.org/ipc-country-analysis/ipc-mapping-tool/). You can access IPC/CH data directly on [HDX](https://data.humdata.org/dataset/global-acute-food-insecurity-country-data) or retrieve the data from their [API](https://www.ipcinfo.org/ipc-country-analysis/api/) and explore latest analyses on their [website](https://www.ipcinfo.org/ipc-country-analysis/en/).

## Signals detection

We generate signals on IPC/CH data when the most recent analyses show an increase in populations in phase 3 and above, comparing the current estimates to the previous analysis and the projections to the current.&#x20;

There are a series of thresholds to ensure signals are only generated if the food insecurity situation reaches a minimum level of severity. Signals are only generated if 20% or more of the population is in phase 3 and above in the current or projected estimates, or if 5% or more of the population is in phase 4 and above. This follows the [20% rule in area phase classification](https://www.ipcinfo.org/ipc-manual-interactive/ipc-acute-food-insecurity-protocols/function-2-classify-severity-and-identify-key-drivers/protocol-23-adhere-to-analytical-parameters/en/).

A signal is automatically generated if there are any populations estimated to be in catastrophic/famine conditions (phase 5), regardless of whether this is an increase from previous analyses.

#### Partial analyses

In some locations, IPC/CH analyses do not cover the entire population or all subnational areas. This can make it difficult to draw comparisons across time. To deal with this, for each IPC/CH analysis, we calculate the percent of the total population covered in that analysis and attempt to detect from the analysis title if it is a partial analysis for a specific subnational area. For example, for the analysis `Madagascar - Acute Food Insecurity Nov 2022 (Grand Sud/Est)` we estimate the coverage is partial, covering only Grand Sud/Est.

When generating signals, we only make comparisons to the previous analysis if we believe that previous analysis covers a similar population. This is done by checking if there is an indication of partial coverage in the title and the percent of population analyzed differs by less than 10%, based on[ IPC/CH standards](https://www.fsinplatform.org/report/global-report-food-crises-2024/#download). If they are not comparable, then we do not compare with the previous current estimates, and only compare current and projected estimates from the analysis.

#### Examples

Below are historical examples that show how signals are generated with IPC/CH data.

[Madagascar](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=1f4d296ccd#MDG): new IPC analysis in early 2022 projected increases in populations in phase 3 and above for Grand Sud/Est.

[South Sudan](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=abf8af12bf#SSD): populations estimated to be in phase 5 triggered an immediate signal.
