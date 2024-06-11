## Email

### Components

Emails are constructed from a base template,
[`hdx_signals_template.html`](components/hdx_signals_template.html).
This template has `{glue}` strings within them where the body of the email can
be inserted, link to the banner image, and campaign URL. The body for the
template is created with the `components` scripts, including various
blocks of HTML for images, text, banners and other content. All of the content
that is used comes from the campaign content generated and stored in the signals
dataset.

To ensure that content is only seen by subscribers, we also wrap a lot of
content in conditional merge logic that is a specific Mailchimp code that
allows us to show email content only to interested subscribers. Read more
on the [Mailchimp webpage](https://mailchimp.com/help/use-conditional-merge-tag-blocks/).

### Mailchimp

**Segmentation**

Segmentation is how we select recipients of emails. The primary function used
for campaign generation is [`custom_segmentation.R`](mailchimp/custom_segmentation.R). It
is used to identify subscribers to the Mailchimp Signals audience, and for every
campaign, calculate who should receive email alerts. This is done by extracting
the Mailchimp audience data and manually calculating all subscribers interested
in any location ISO3 in the campaign and the specific indicator. This is manually
done because the Mailchimp API does not allow complex logical combinations, so
it's not possible to create segments through Mailchimp logic.

**Other utilities**

Other scripts are designed to help us load up content into Mailchimp and delete
it when necessary. We store all content in folders for specific indicators,
with the folder name in `input/indicator_mapping.parquet`. These are actually 3
separate folders on the Mailchimp system to store files, templates, and campaigns,
they just have the same name for the same dataset.
