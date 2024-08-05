## Source code

The source code for Signals is contained within `src`. The code is all code used
for production runs in GitHub Actions, and does not contain code to update static
assets or test. Visit the folders themselves
for more detailed documentation of the code within.

- [email](email/README.md): Automatically construct an email in HTML and read from
and save to Mailchimp.
- [images](images/README.md): Create and save out plots and maps for use in emails.
- [indicators](indicators/README.md): Download, wrangle, and scan indicator data.
- [repo](repo/README.md): Code to automate documentation and other repository checks.
- [signals](signals/README.md): Generate and check signals, used in all indicator scanning.
- [utils](utils/README.md): General utilities used across the rest of `src` and `src-static`.
