# CERF Global Monitoring System

This repository contains the code base for the CERF Global Monitoring System (GMS).

## Introduction

The CERF GMS is an early warning system designed to generate alerts on
potentially new or worsening crises. Alerts are intended to be just an initial
signal that a crisis should be further explored and see if action, such as
rapid response, is justified.

### Overall system

The system works by updating a series of indicators data, generating alerts when
abnormalities are detected, and sending emails for any new and recent alerts.
Refer to the [system document](/src) for full details.

### Testing and development

For testing email design and interactive development, testing whether the system
runs properly, refer to the [testing documentation](/src/test).

### Changes

All changes to the repository are tracked in [NEWS.md](NEWS.md).

----

License: [GPLv3](LICENSE)
