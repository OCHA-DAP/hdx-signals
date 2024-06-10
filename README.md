<img src="https://github.com/OCHA-DAP/hdx-signals/assets/HDXSignalsLogo_V2.png" width=200>

HDX Signals is a product of [HDX](https://data.humdata.org) that monitors key
datasets and generates automated emails when significant, negative changes
are detected. 

Find all code and technical documentation behind HDX Signals in
this repository. Read more about HDX Signals and sign up to receive signals direct
in your inbox on [our website](https://data.humdata.org/signals). See our
methods documentation for details on datasets included and when signals
are generated.

## Overview

The code is structured as below. Refer to the directories for specific details.

- [src](/src): All source code for scanning all datasets, generating signals content,
sendin emails through Mailchimp, and interacting with the cloud store.
- [src-static](/src-static): Source code for generating static assets such as
metadata for locations covered in Signals, spatial files for use in maps, and other
resources utilized in [src](/src).
- [test](/test): Unit testing utilities for code and manual testing to explore changes
to visual design in the system.
- [.github][/.github]: Workflows that automate data scanning and other processes.

### Environment

[{renv}](https://github.com/rstudio/renv) is used for package management.
[{box}](https://github.com/klmr/box) is used for module importation. More details
on their usage in contributing. See all required environment variables and brief
descriptions in [ENVIRONMENT.md](ENVIRONMENT.md).

### Contributing 

See [CONTRIBUTING.md](CONTRIBUTING.md) for details on coding practices for
the system and how to contribute.

### Changes

All changes to the repository are tracked in [CHANGES.md](CHANGES.md).

----

License: [GPLv3](LICENSE)
