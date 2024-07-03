## GitHub Actions workflows

Automated workflows are defined in the standard `.yaml` files in `.github/workflows`. These
generally fall into 3 categories:

- `monitor_...yaml`: Monitoring workflows that scan datasets for signals and generate new
signals when necessary.
- `update...yaml`: Update workflows that can be run to update various static assets.
- Miscellaneous: other useful workflows, typically supporting the maintenance of HDX Signals
through automated testing, linting, or notifying the team on Slack when signals are detected.
