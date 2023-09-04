## Email

When new alerts are generated, they are sent out via email to the list of
recipients defined in the
[email_recipients Google Sheet](https://docs.google.com/spreadsheets/d/1w4DG-CXkcslphWYTnwhLNwDpCPzVu2hREjH_i5cBhbU/edit?usp=sharing).
The emails are sent using the {blastula} package, with the RMarkdown files
in this folder composing the core of the email. An individual email is sent
out for each new *alert type*, with multiple country alerts within a single
email body.

1. `email.Rmd` composing the primary body of the email. It expects already to
have in the global environment the variables `flag_type`, `flag_source`, and
`df_email`. This is automatically handled in the [email utilities](/src/utils/email.R)
where the `send_email()` function is passed these explicitly. These are then
used to programmatically define the email structure, such as reference text
and other parts of the body of the email.

2. `alert_section.Rmd` generates the individual section for each country being
alerted, including generic text describing the alert, a plot, and if included,
NLP generated situation summary.

3. `summary.Rmd` is the optional section that includes the NLP summary, and is
split off as a separate RMarkdown file so it is not included in the output if
no summary exists.

### Email history

A record of previous emails generated is stored in `flags_emailed.gdsheets`, which
simply stores records from `flags_total.gdsheets` whenever they are used to generate
an email, with a single additional column recording the date of the email.
