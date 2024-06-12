# Agricultural Hotspots

Signals are sent when the hotspot classification for a location increases.

## Source

[Anomaly Hotspots of Agricultural Production (ASAP)](https://agricultural-production-hotspots.ec.europa.eu) data produced by the Joint Research Centre. ASAP is an online decision support system for early warning about hotspots of agricultural production anomaly (crop and rangeland), developed for food security crises prevention and response.

The ASAP system produces automatic warnings at a subnational level, using a range of remote sensing and earth observation data to monitor agricultural conditions. These subnational warnings are converted into national level hotspots analyses by a team of analysts. Each location is classified as `No hotspot`, `Hotspot`, or `Major hotspot`.&#x20;

These classifications are produced on a monthly basis, covering a set of locations at risk of food insecurity. Read more about the ASAP methods in their [documentation](https://agricultural-production-hotspots.ec.europa.eu/documentation.php).

Access ASAP data directly on [HDX](https://data.humdata.org/dataset/asap-hotspots-monthly) and find additional datasets and context on the [ASAP website.](https://agricultural-production-hotspots.ec.europa.eu/download.php)

## Signals detection

Signals are sent when new agricultural hotspots are identified. This occurs when a location is classified as a `Hotspot` but was `No hotspot` in the previous month, or is now `Major hotspot` and was either `Hotspot` or `Hotspot` in the previous month.

#### Examples

Below are historical examples that show how signals are generated with ASAP data.

[Sudan](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=10b16f9587#SDN): classified as `Hotspot` in April 2023 and then classified as a `Major hotspot` in May 2023.&#x20;

[Mozambique](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=dab7a5f66c#MOZ): classified as `No hotspot` in December 2020, but `Hotspot` in January 2021.

