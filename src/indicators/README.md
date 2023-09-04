## Indicators

Early warnings are run from different sets of global indicators. All of the
analysis code is contained under the `src` folder. Each indicator has
its own folder with a `update_(...).R` file that downloads raw data for the
indicator, wrangles into a usable file for visualizations and alerts, and
generates flags for that indicator. These files are all standalone files that
can be run without any other external inputs, with all outputs being saved
direct to Google Drive. Once each individual indicator is updated, 
`src/send_alerts.R` pulls together the alerts data and sends out emails when
new alerts are detected. 

The specific methodologies used for each of the indicators within the project
are contained in their own specific README files, linked below. The
output for all data sources are three files:

1. `flags_{type}.gdsheets`: a flagging dataset that has a row of flags
with a start date, end date, and explanatory message. There is only one
output flag for each source of data.
2. `raw_{type}.gdsheets`: the raw data from the data source that can be used to
contextualize the alert. Typically only useful in tabular format.
3. `wrangled_{type}.gdsheets`: the wrangled data from the data source that can
be used to contextualize the alert, and is generally ready for plotting.

The current indicators included in the CERF GMS are:

- [Internal Displacement Monitoring Centre (IDMC) displacement data](src/indicators/idmc/README.md)
- [Integrated Food Security Phase Classification (IPC) food security data](src/indicators/ipc/README.md)
- [WHO AFRO cholera data](src/indicators/cholera/README.md)
