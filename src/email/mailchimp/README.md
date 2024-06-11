## Mailchimp 

Functions to interact with the Mailchimp API.

### Segmentation

Segmentation is how we select recipients of emails. The primary function used
for campaign generation is [`custom_segmentation.R`](custom_segmentation.R). It
is used to identify subscribers to the Mailchimp Signals audience, and for every
campaign, calculate who should receive email alerts. This is done by extracting
the Mailchimp audience data and manually calculating all subscribers interested
in any location ISO3 in the campaign and the specific indicator. This is manually
done because the Mailchimp API does not allow complex logical combinations, so
it's not possible to create segments through Mailchimp logic.

These utilities are used in `custom_segmentation()` to support calculations.

- [`audience.R`](audience.R): Retrieve audience details, such as members, potential
interests (e.g. regions, datasets).
- [`segments.R`](segments.R): Create and retrieve segments (groups of subscribers)
that can be used to send emails to specific recipients.
- [`tags.R`](tags.R): Access and create manual tags for subscribers, which can
be used to send emails to people tagged (e.g. what we do with cholera).

### Content

Other utilities are designed to help us load up content into Mailchimp and delete
it when necessary. We store all content in folders for specific indicators,
with the folder name in `input/indicator_mapping.parquet`. These are actually 3
separate folders on the Mailchimp system to store files, templates, and campaigns,
they just have the same name for the same dataset.

- [`templates.R`](templates.R): Create and save templates to Mailchimp.
- [`images.R`](images.R): Upload images to Mailchimp.
- [`campaigns.R`](campaigns.R): Create and send Mailchimp campaigns.
- [`delete.R`](delete.R): Delete files, templates, and campaigns from Mailchimp.
- [`folders.R`](folders.R): Find folder IDs so storage is organized.

### Base API

All of this uses the simple [`base_api.R`](base_api.R) helper.
