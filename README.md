# CERF Global Monitoring and Alert System (GMAS)

This repository contains the code base for the CERF GMAS.

## Introduction

The CERF GMAS is an early warning system designed to generate alerts on
potentially new or worsening crises. Alerts are intended to be just an initial
signal that a crisis should be further explored and see if action, such as
rapid response, is justified.

### Overall system

The system works by updating a series of indicators data, generating alerts when
abnormalities are detected, and sending emails for any new and recent alerts.
You can get an overview of the codebase in [src](/src).

### Testing and development

For testing email design and interactive development, testing whether the system
runs properly, refer to the [testing documentation](/src/test).

----

License: [GPLv3](LICENSE)
