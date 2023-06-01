# Testing

Testing for the global monitoring system is currently designed around testing
shifts in email design without sending out emails to all recipients of the alert
system. This is designed around 2 scripts:

- `test.R`: Sends out a test email to recipients defined on the Google Drive
file
[email_recipients.csv](https://drive.google.com/file/d/1k1NawE8Kpp8fJYi9kcdgEd0lWy_PTmIe/view?usp=share_link),
only to recipients with `TRUE` in the `test` column. The email data is defined
by [flags_test.csv](https://drive.google.com/file/d/1HYXXCkFpujlozN_vBSUE8DjWDnkD1z_H/view?usp=share_link)
which is static and contains flags in the `email` column to generate a flag.

- `generate_test_csv.R`: If the format of `flags_total.csv` changes, such as by
changing the message format for a specific dataset or adding a dataset, we need
to reflect those changes within `flags_test.csv`. This script re-generates the
`flags_test.csv` file.
