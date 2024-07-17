## Static assets source code

A variety of static assets are used in HDX Signals. These are all stored in
the HDX Signals prod container on Azure, in the `input` folder. Some of these
assets are produced through source code in this folder, which can be updated
with `update_assets.yaml` GitHub Action. A few of the assets are provided
manually. The assets used in Signals are briefly described below, including how
they are updated.

### Assets

- `adm0`: Admin 0 polygon layers for use as a base layer in maps. Each location
has an `adm0/{ISO3}.geojson` file that can be used. Updated in
`update_adm0_sf.R`
- `centroids`: Admin 0 centroids that are added to the final Signals dataset.
These are not published to HDX, and are just used for the webmap on the HDX
website. Updated in `update_centroids_sf.R`.
- `cities`: Cities spatial data for use in maps. Updated in
`update_cities_sf.R`.
- `acled_info.parquet`: Data on the start date of ACLED coverage for locations
and the web URL to the location page. Updated in `update_acled_info.R`.
- `asap_iso3.parquet`: ASAP0 to ISO3 mapping file. Used in `location_codes.R`.
- `gaul1_asap_FAO_Mapping_v8.csv`: CSV file provided directly by JRC to map
ASAP0 IDs to GAUL codes, which can then map to ISO3. Used to created
`asap_iso3.parquet`.
- `hrp_locations.parquet`: Vector of ISO3 codes for HRP locations. Script
will need adjustment when new HRP locations are created. Updated in
`update_hrp_locations.R`.
- `idmc_urls.csv`: Data on the URL for IDMC locations. Manually provided by
the IDMC via email and uploaded to the Azure container.
- `indicator_mapping.parquet`: Mapping from `indicator_id` to various other
information necessary for email generation, such as Mailchimp settings and
subject line and title for emails. Updated in
`update_indicator_mapping.R`
- `iso3_map_settings.json`: Map settings that determine legend placement for
dynamic maps. Updated in `update_iso3_map_settings.R`
- `locations_metadata.parquet`: Metadata for locations, such as their name,
regions, centroids, and other information that are joined up to create the final
Signals dataset and metadata file on HDX. Updated in `update_locations_metadata.R`.
- `locations.parquet`: All ISO3 codes for locations covered by HDX Signals.
Updated in `update_locations.R`.
- `un_geodata_complex.geojson`: UN admin 0 polygons, used to create the base layers
in `adm0`. Secured privately, and updates will need to be made upon request to the
UN Geo Hub, where only the simplified boundaries are available for public
downloaded.
