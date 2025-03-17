# Environment variables

Below are the environment variables used in this repository. Get in touch with
the maintainers if you do not have them and feel you should be given access. The easiest
way to define them for use in R is in an `.Renviron` file, either global or local.
You can directly do this in R with `usethis::edit_r_environ()`.

## Generating signals

Environment variables are used to determine how `generate_signals()` works. These
are all logical variables that are read internally to determine how the underlying
functions behave and the final output returned. The values should be `TRUE` or `FALSE`.

- `HS_LOCAL`: if `TRUE`, prevents functions from writing to the Azure containers,
sending emails with Mailchimp, or calling the OpenAI endpoint. All other functionality
occurs, but the outputs thus remain local to the running machine. Useful for testing
and interactive development.
- `HS_DRY_RUN`: if `TRUE`, `generate_signals()` does a dry run, which ensures
existing signals or monitoring is not interrupted. Use this for testing and interactive
development because `HS_DRY_RUN` ensures that signals are available for testing.
With `HS_LOCAL` and `HS_DRY_RUN` set to `TRUE`, you can test changes
to visuals, other email content, or just ensure that campaigns can still be generated
with source code changes. With `HS_LOCAL` set to `FALSE`, you can generate test
signals and load them to Azure and Mailchimp.
- `HS_FIRST_RUN`: if `TRUE`, generates the first run for an indicator that is the
historical signals. `generate_signals()` will throw an error if you try to do a
first run when data already exists, or if you try to monitor data without having
done the first run. You can use in conjunction with `HS_DRY_RUN` to do test
first runs.

## Reading and writing data

Data is stored in Azure storage containers, and final output datasets are also
transferred automatically from Azure into [HDX](https://data.humdata.org). 

### Azure

To read to and from the Azure storage containers, you need the following two permissions:

- `DSCI_AZ_BLOB_PROD_SAS_WRITE`: SAS token for access to the production stage container.
- `DSCI_AZ_BLOB_DEV_SAS_WRITE`: SAS token for access to the development stage container. Not
currently used, but historically some data was stored on `dev`.

### HDX

Data is pushed to HDX using pipelines in
[hdx-signals-alerts](https://github.com/OCHA-DAP/hdx-signals-alerts). This
requires:

- `HS_HDX_BEARER`: Bearer token to trigger the webhook to transfer from Azure to
HDX.

## Run notifications in Slack

Notifications of dataset runs and updates are done by accessing the GitHub API to
get workflow status and sending notifications hrough the Slack API to
the team Slack channels. This requires:

- `HS_SLACK_URL`: URL for the `hdx-signals-bot` channel, used for actual notifications.
- `HS_SLACK_URL_TEST`: URL for the `hdx-signals-bot-testing` channel, used to
test changes to notifications.
- `GH_TOKEN`: Bearer token to get access to workflow runs from GitHub.

## Sending emails

Saving content, creating templates and campaigns, and sending emails through Mailchimp.
The HDX Signals sending address and link to the feedback survey are also environment
variables to prevent hardcoded storing on GitHub and potential spam mail.
This requires:

- `MAILCHIMP_API_KEY`: API key with access to the `HDX Signals` audience.
- `HS_EMAIL`: HDX Signals email used as the sender in any Mailchimp campaigns.
- `HS_SURVEY_LINK`: Link to the Mailchimp feedback survey contained at the bottom of
all emails.

## OpenAI

OpenAI's API is used to generate text summarizations. This requires:

- `OPENAI_API_KEY`: Project API key with access to chat completion models and
`gpt-4o` specifically. This incurs costs, so there is an organisation key
available upon request and approval.

## Datasets

The datasets scanned by HDX Signals requires a range of environment variables,
primarily to provide access to APIs.

### `acled_conflict`

- `ACLED_ACCESS_KEY`: ACLED access key provisioned upon request to ACLED.
- `ACLED_EMAIL_ADDRESS`: Email address used to register the key.

### `idmc_displacement_{shock}`

- `IDMC_API`: URL endpoint to the IDMC IDU. Not an actual key. Read more in
[{idmc}](https://github.com/OCHA-DAP/idmc).

### `ipc_food_insecurity`

- `IPC_API_KEY`: IPC API key. Read more in [{ripc}](https://github.com/OCHA-DAP/ripc).

### `who_cholera`

- `CERF_CHOLERA_DATA`: link to cholera data scraped by CERF from WHO health bulletins
