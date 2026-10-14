# geomarker 0.1.1.9000

- NARR downloads now use NOAA PSL's THREDDS file server. Existing downloads
  from the previous URL are not reused because cache filenames include the URL.
  NARR skips ETag header requests and retries HTTP 429 responses up to four
  times with increasing waits.

- `get_smoke_summary()` can now return every intersecting smoke-plume density
  or add density scores across intersecting plumes, while retaining the maximum
  density as its default summary.

# geomarker 0.1.1

- `get_tiger_bg()` now preserves input order across state-grouped lookups,
  including duplicate and missing cells.

# geomarker 0.1.0

- Initial release.
