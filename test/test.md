# Testing

Testing for the global monitoring system is currently designed around testing
shifts in email design without sending out emails to all recipients of the alert
system. This is designed around 2 scripts:

- `test.R`: Sends out a test email to recipients defined on the Google Drive
file
[email_recipients.gdsheets](https://docs.google.com/spreadsheets/d/1w4DG-CXkcslphWYTnwhLNwDpCPzVu2hREjH_i5cBhbU/edit?usp=sharing),
only to recipients with `TRUE` in the `test` column. The email data is defined
by [flags_test.gdsheets](https://docs.google.com/spreadsheets/d/19lS5XT8I7ijD7rRsWZTI9qH46-qpHjpY9KIqqtLUyPE/edit?usp=sharing)
which is static and contains flags in the `email` column to generate a flag.

- `generate_test_file.R`: If the format of `flags_total.gdsheets` changes, such as by
changing the message format for a specific dataset or adding a dataset, we need
to reflect those changes within `flags_test.gdsheets`. This script re-generates the
`flags_test.gdsheets` file.
