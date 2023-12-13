## Alerts

The `send_alerts.R` script pulls together all of the alerts data that is
updated with the [indicators scripts](/src/indicators). Once these have been run,
it pulls together all flagging data to create total flags.

### Outputs

1. `output/flags_total.parquet`: all total flags generated from the sourced indicators.
Each row corresponds to a flag, with information indicating the country being
flagged, the type and source of the flag, and information related to what
has generated the latest flags.
2. `flags_total_daily.gdsheets`: the same dataset as `flags_total.gdsheets`, but where
each flag has a row for the days between `start_date` and `end_date`. Used for
filtration on the PowerBI dashboard. **This output will be deprecated once CERF
switches to a different system.**

### Email generation

Beyond outputting these datasets, emails are generated for any new alerts,
described in the [email documentation](/src/email).
