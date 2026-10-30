# geomarker 0.1.1.9000

- Elevation, traffic, and MERRA-2 assets now have independent, immutable data
  releases. Package versions pin exact data revisions. The files are unchanged,
  but their new URLs require fresh downloads instead of reusing legacy cache
  entries. Existing assets remain available for older package versions.

- NARR downloads now use NOAA PSL's anonymous FTP server. Existing downloads
  from the previous URL are not reused because cache filenames include the URL.

- `get_smoke_summary()` can now return every intersecting smoke-plume density
  or add density scores across intersecting plumes, while retaining the maximum
  density as its default summary.

# geomarker 0.1.1

- `get_tiger_bg()` now preserves input order across state-grouped lookups,
  including duplicate and missing cells.

# geomarker 0.1.0

- Initial release.
