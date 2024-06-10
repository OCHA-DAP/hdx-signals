## Source code

The source code for Signals is contained within `src`. The code is all code used
for production runs in GitHub Actions, and does not contain code to update static
assets or test. Visit the folders themselves
for more detailed documentation of the code within.

- [signals](signals/README.md): Generate and check signals, used in all indicator scanning.
- [email](email/README.md): Automatically construct an email in HTML and read from
and save to Mailchimp.
- [images](images/README.md): Create and save out plots and maps for use in emails.
- [indicators](indicators/README.md): Download, wrangle, and scan indicator data. 
- [utils](utils/README.md): General utilities used across the rest of `src` and `src-static`.

### `generate_signals()`

The primary workhorse of all of `src` is `generate_signals()` in 
`src/signals/generate_signals.R`. The function is used in all `indicator` scripts
to generate signals when negative changes are detected, create visualisations with
`images` functions, emails from `email`, and using many of the utilities in `utils`.

Parsing through a run of `generate_signals()` is a good place to see how all of the
code in `src` comes together to create the final product.
