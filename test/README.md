## Testing

### Test emails

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

### Testing the system

For testing that the scripts update and run properly, the global monitoring system
is also designed to not save out data, ping the OpenAI API, or send emails
if the environment variable `GMAS_TEST_RUN` is set to `TRUE` or does not exist.
This means that if you are interactively testing and building within the environment,
you will not accidentally send emails or anything else, you can feel safe that
you will not change external files or engage external users.

The `global-monitoring` GitHub Actions workflow allows you use to do a manual run
with the `GMAS_TEST_RUN` set to `TRUE` so you can test that the workflow runs
without error, but without updating any files. By default, the CRON job
`global-monitoring` workflow sets `GMAS_TEST_RUN` to `FALSE`, so should be the
only time that files are saved or emails are sent.
