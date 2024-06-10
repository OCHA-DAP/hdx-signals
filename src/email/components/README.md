## Email components

Emails are constructed from a base template, [`hdx_signals_template.html`](hdx_signals_template.html).
This template has `{glue}` strings within them where the body of the email can
be inserted, link to the banner image, and campaign URL. The creation of the
email body is created by [`email_body.R`](email_body.R), and then injected
along with URLs for the campaign and banner in [`create_template.R`](create_template.R).
All of this is done in `src/signals/create_campaigns.R` using the campaign content
generated in `src/signals/generate_signals.R`.

### Content blocks

Content blocks for the email are put together in `email_body.R`, and the individual
blocks are generated in these helper functions.

- [`banner_block.R`](banner_block.R): Banner block for top of email.
- [`image_block.R`](image_block.R): Block to contain images.
- [`intro_block.R`](intro_block.R): Intro block with the location names and headlines.
- [`line_block.R`](line_block.R): Adds a separator line to the email.
- [`location_block.R`](location_block.R): Full location block content that puts
together all images and text from the campaign content, using other content blocks
together.
- [`summary_block.R`](summary_block.R): AI summary text block.
- [`text_block.R`](text_block.R): Generic text block used for various text blocks
in `country_block.R` and `intro_block.R`

### Utilities

- [`missing.R`](missing.R): Simple function that checks if a string is `""` or
`NA` to conditionally remove content blocks if input missing.
- [`conditional_merge.R`](conditional_merge.R): Mailchimp conditional merge
logic used to ensure that recipients only see content they subscribed to.
